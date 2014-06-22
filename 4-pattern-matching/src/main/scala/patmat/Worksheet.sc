import patmat.Huffman._

createCodeTree("alibaba".toList).toString()

createCodeTree("the quick brown fox jumps over the lazy dog".toList).toString()

val frenchOther ="j'aime aller sur le bord de l'eau les jeudis ou les jours impairs"
createCodeTree(frenchOther.toList).toString
encode(createCodeTree(frenchOther.toList))(frenchOther.toList)
val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
decode(t1, List(0,1)).mkString
decodedSecret.mkString
encode(t1)("ab".toList)
encode(t2)("dab".toList)
convert(t1)
convert(t2)

