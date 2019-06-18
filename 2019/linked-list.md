---
date: 2019-06-18
tags: programming java algorithm
---

# Reverse

Reversing a [Linked List][1] in Java with old-fashioned pointers.

```java
public class LinkedList<T> {
    private Node<T> head;

    public LinkedList(Node<T> head) {
        this.head = head;
    }

    public static class Node<T> {
        private T value;
        private Node<T> next;

        public Node(T value, Node<T> next) {
            this.value = value;
            this.next = next;
        }
    }

    public void reverse() {
        Node<T> curr = head;
        if (curr == null) {
            return;
        }

        Node<T> prev = null;
        Node<T> next = curr.next;
        while (next != null) {
            curr.next = prev;
            prev = curr;
            curr = next;
            next = curr.next;
        }

        curr.next = prev;
        head = curr;
    }

    @Override
    public String toString() {
        return "[" + toStringInternal(head) + "]";
    }

    private String toStringInternal(Node<T> node) {
        if (node == null) {
            return "";
        } else if (node.next == null) {
            return node.value.toString();
        } else {
            return node.value + ", " + toStringInternal(node.next);
        }
    }
}
```

[1]: https://en.wikipedia.org/wiki/Linked_list