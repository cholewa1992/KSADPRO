trait Node[A]
case class Leaf[A] (elem:A)
case class Node2[A] (elem1: Node[A], elem2: Node[A])
case class Node3[A] (elem1: Node[A], elem2: Node[A], elem3: Node[A])
case class Node2[A](l:A,r:A)


/*
Empty -> no content
Single -> 1 element
Deep -> internal nodes in the tree [prefix digit, middle deep node, suffix digit]
Digit -> a leaf holding 1-4 elements
------
Node2, Node3 -> binary temporary tree nodes

invariant
a ndode at depth n of the ft holds trees of hight n


                        deep
                        /|\
      prefix(1,2,_,_)  / | \  suffix (3,4,5,_)
                        /|\
      prefix(n,n,_,_)  / | \ suffix (n,n,_,_)
             | |         e           | |
             |/|\                    |/|\
            / \                     / \
*/
