import groovy.transform.ToString

def text = new File('input.txt').text.trim()
//text = new File('input2.txt').text.trim()

@ToString(includeNames = true)
class Node {
    int childrenCount
    int metadataCount
    List<Integer> metadata = []
    List<Node> children = []
    Integer value = null

    Integer calculateMeta() {
        if (value != null) {
            return value
        }
        if (metadataCount == 0) {
            value = 0
        } else if (childrenCount == 0) {
            value = metadata.sum()
        } else {
            value = metadata.findAll { it <= childrenCount }
                    .collect { children[it - 1].calculateMeta() }
                    .sum() ?: 0
        }
        return value
    }
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

long metadatadSum = root.calculateMeta()
println(metadatadSum)