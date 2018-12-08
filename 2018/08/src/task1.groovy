import groovy.transform.ToString

def text = new File('input.txt').text.trim()

@ToString
class Node {
    int childrenCount
    int metadataCount
    List<Integer> metadata = []
    List<Node> children = []
}

def values = text.split(/ /).collect { it as int }.reverse()

def readNode(List<Integer> values) {
    int childrenCount = values.pop()
    int metadataCount = values.pop()
    Node node = new Node(childrenCount: childrenCount, metadataCount: metadataCount)
    (0..<childrenCount).each {
        Node child = readNode(values)
        node.children << child
    }
    (0..<metadataCount).each {
        int metadata = values.pop()
        node.metadata << metadata
    }
    return node
}

Node root = readNode(values)

assert values.empty

List<Node> toCheck = [root]
long metadatadSum = 0
while (toCheck) {
    Node node = toCheck.pop()
    toCheck.addAll(node.children)
    metadatadSum += node.metadata.sum()
}
println(metadatadSum)