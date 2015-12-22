#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

execname := '$RCSfile: hzip.st,v $' =~ '.*: (.*),v.*' at: 1.
nl := Character nl.
queue := SortedCollection new.
|decodingtree|

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]
   

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
]

Object subclass: BitStream [
   |bitIndex byte myStream|
   BitStream class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   init: fileStream [
      myStream := fileStream.
      bitIndex := 1.
   ]
   nextBit [
      bitIndex = 1 ifTrue: [
         byte := myStream next.
         bitIndex := 9.
      ].
      bitIndex := bitIndex - 1.
      ^byte value bitAt: bitIndex
   ]
   atEnd [
      ^bitIndex = 1 and: [myStream atEnd]
   ]
]

freqtable := ZeroArray new: 257.
encodetable := ZeroArray new: 257.
freqtable at:256 put:1.
encodetable at:256 put:'0'.

Object subclass: Leaf [
   |char count leafNode|
   char [ ^ char ]
   count [ ^ count ]
   leafNode [ ^ leafNode ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount leaf: aLeaf[
      |result|
      result := super new.
      result setChar: aChar andCount: aCount andLeaf: aLeaf.
      ^result
   ]

   setChar: aChar andCount: aCount andLeaf: aLeaf[
      char := aChar.
      count := aCount.
      leafNode := aLeaf.
   ]
   
   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   depthFirst: visitor prefix: string [
      visitor value: char value: string.
   ]

   postOrder: visitor [
      visitor value: char value: leafNode.
   ]
]


Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight 
   leaf: aLeaf[
      |result|
      result := super new: aChar count: aCount leaf: aLeaf.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   left  [ ^ left]
   right [ ^ right]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   depthFirst: visitor prefix: string [ 
      left depthFirst: visitor prefix: string, '0'.
      right depthFirst: visitor prefix: string, '1'.
      
   ]
   
   postOrder: visitor [
      left postOrder: visitor.
      right postOrder: visitor.
      visitor value: char value: leafNode.
   ]
]


Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
         myStream nextPutByte: currentByte.
         self clearByte.

   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [
        self flushByte].
   ]
   writeByte: byte [
      myStream nextPutByte: byte.
   ]
]

Character extend [
   padding [
      ^ (Character space < self) & (self <= $~)
   ]
   digits[
      ^ (self asInteger) > 15
   ]
   printFormat [
      self padding ifTrue: [^' ', self asString, ' ']             
      ifFalse: [ 
         self digits ifTrue:[
            ((self asInteger) = 256) ifTrue:[ ^ 'EOF']ifFalse:[
               ^ 'x', 
               ((self asInteger printStringRadix: 16) copyFrom: 4 )
            ]
        ]
        ifFalse:[
           ^ 'x0', 
           ((self asInteger printStringRadix: 16) copyFrom: 4)
        ]
      ]           
   ]
]

|infile infile2 outfile argcount|

dFlag := false.
cFlag := false.
tFlag := false.
uFlag := false.

argcount:=0.

Smalltalk arguments: '-d -t -c -u'
do: [:opt :arg |
  
   
   (opt = $d) ifTrue: [
    ].
    
   (opt = $t) ifTrue: [
   tFlag := true.
    ].

  (opt = $c) ifTrue: [
   cFlag := true.
   ].

  (opt = $u) ifTrue: [
   uFlag := true.
   ].
  
  (arg ~= nil) ifTrue: [
    
     argcount = 0 
        ifTrue:[
          infile := FileStream open: arg
                    mode: FileStream read.
          infile2 := FileStream open: arg
                    mode: FileStream read.
        ]
        ifFalse:[
            outfile := FileStream open: arg
                       mode: FileStream write.].
  
     argcount := argcount + 1.
      ] 
   
].

argcount = 0 ifTrue: [
   stderr << execname << ' Input file is needed.' << nl.
   stderr flush.
   ObjectMemory quit: 1.
].

tFlag | cFlag ifTrue:[
    [infile atEnd not] whileTrue: [
       |ordChar|
       ordChar := infile next asInteger.
       freqtable incr: ordChar.
    ].

    freqtable keysAndValuesDo: [:key :count |
       (count notNil and: [count > 0]) ifTrue: [
          queue add: (Leaf new: key count: count leaf: true).
       ]
    ].

    [queue size >1 ] whileTrue: [
      
       |leftchild rightchild|
       leftchild := queue removeFirst.
       rightchild := queue removeFirst.
       newtree := ( Tree new: (leftchild char) +  (rightchild char)
                          count: (leftchild count) +  (rightchild count)
                          left: leftchild right: rightchild
                    leaf: false).
       queue add: newtree.
    ].
    decodingtree:=queue removeFirst.


    decodingtree depthFirst: [:char :string |
      string ~= '' ifTrue: [
         encodetable at: char put: string.
      ]

    ] prefix: ''.
   
   tFlag ifTrue:[
        stdout <<nl<< '------------decoding tree--------------' << nl.
        stdout << decodingtree << nl.

        stdout <<nl<< '------------encoding table-------------' << nl.
        encodetable keysAndValuesDo: [:key :count |
        (count notNil and: [count > 0]) ifTrue: [
           stdout <<  key asCharacter printFormat << '     '
                  << (freqtable at: key) <<'    '<<count
                  << nl.
        ].
       ].
   ].

   cFlag  ifTrue:[ 

   outfile~=nil ifTrue:[

       outBytes := OutBits new: outfile.
       
       decodingtree postOrder: [:char :leafNode|
         leafNode = true ifTrue: [
            outBytes writeBit: 0.
            1 to: 8 do: [:i |
               currentBit := char bitAt: i.
               currentBit = 1 ifTrue: [
                  outBytes writeBit: 1.
               ] ifFalse: [
                  outBytes writeBit: 0.
               ].
               
            ].  
            char asInteger = 256 ifTrue: [
               outBytes writeBit: 1.
            ].
            char asInteger = 0 ifTrue: [
               outBytes writeBit: 0.
            ].
         ] ifFalse: [
            outBytes writeBit: 1.
         ].
       ].
       outBytes writeBit: 1.
       
       [infile2 atEnd not] whileTrue: [
         |ordChar2|
         ordChar2 := infile2 next asInteger.
         code:= (encodetable at: ordChar2). 
         1 to: (code size) by: 1 do: [:index |
           bit:= ((code at: index) asInteger) - 48.
           outBytes writeBit: bit.
            "stdout <<bit."
         ].
       ].

      eof:= (encodetable at: 256).
      1 to: (eof size) by: 1 do: [:index |
            bit:= ((eof at: index) asInteger) - 48.
            outBytes writeBit: bit.].     
      outBytes flushByte.
   ].   
   ].
].
stack := OrderedCollection new.
uFlag ifTrue:[
   
    fileBits := BitStream new: infile.
    
    [fileBits atEnd] whileFalse: [
   
        determineBit := fileBits nextBit.

       determineBit = 0 ifTrue: [
         byteValue := 0.
         1 to: 8 do:[:i | 
            byteBit := fileBits nextBit.

            byteValue := byteValue bitAt: i put: byteBit.
         ].
         byteValue = 0 ifTrue: [
            scbit := fileBits nextBit.
            scbit = 1 ifTrue:[
               byteValue := 256.
            ].
         ].
         stack add: (Leaf new: byteValue count: -1 leaf:true).

       ]
       ifFalse:[
         
           stack size > 1 ifTrue: [
            |leftchild rightchild|
         
            leftchild := stack removeLast.
            rightchild := stack removeLast.
            newtree := ( Tree new: -1
                          count: -1
                          left: rightchild right: leftchild
                          leaf: false).
            stack add: newtree.
            ] ifFalse: [
               decodingtree:=stack removeLast.

               decodingtree depthFirst: [:char :string |
                  string ~= '' ifTrue: [
                     encodetable at: char put: string.
                  ]
               ] prefix: ''.
               "encodetable inspect."
               [fileBits atEnd] whileFalse: [
                  tempBit := fileBits nextBit asString.
                  findIt := 0.
      
                  [findIt = 0 ]whileTrue:[
                     "stdout << tempBit << ' '."
                     0 to: 256 do:[:i | 
                        tempValue := encodetable at: i.

                           tempValue = tempBit ifTrue:[
                              findIt := 1.
                              i = 256 ifTrue:[
                                 [fileBits atEnd] whileFalse:[
                                    fileBits nextBit asString.
                                 ]. 
                              ]ifFalse:[
                                 stack add: i.
                              ].
                           ].
                     ].
                     findIt = 0 ifTrue:[
                        tempBit1 := fileBits nextBit asString.
                        "stdout << tempBit1 << ' '."
                        tempBit := tempBit, tempBit1.
                     ].

                  ].
               ].
            ].
         ].

    ].
    
].

uFlag ifTrue:[
   outBytes := OutBits new: outfile.
   outBytes1 := OutBits new: stdout.
   [stack size > 0] whileTrue: [
      junk := stack removeFirst.
      outfile = nil ifTrue: [
         outBytes1 writeByte: junk
      ] ifFalse: [
         outBytes writeByte: junk. 
      ].
   ].
].

infile close.
