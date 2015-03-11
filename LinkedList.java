class LinkedList {
  public static void main(String[] args) {
    MyList<Integer> lst = new MyList<Integer>();
    lst.push(1);
    lst.push(2);
    lst.push(3);
    lst.print();
    lst.reverse();
    lst.print();
    
    MyList<Integer> lst2 = new MyList<Integer>();
    lst2.push(1);
    lst2.print();
    lst2.reverse();
    lst2.print();

    // copy a list
    MyList<Integer> lst3 = new MyList<Integer>(lst);
    lst.push(0);
    lst3.print();
    lst.print();
  }
}

class MyList<T> {
  private Node<T> head;
  
  public MyList() {
    head = new Node<T>(null);
  }
  
  // does this copy constructor take constant time or linear time?  
  public MyList(MyList<T> lst) {
    this.head = lst.head;
  }
  
  public void push(T val) {
    Node newHead = new Node<T>(val);
    newHead.setNext(head);
    head = newHead;
  }
  
  public T pop() {
    T val = head.getValue();
    // head.setNext(null);
    head = head.getNext();
    return val;
  }
  
  public T peek() {
    return head.getValue();
  }
  
  private void reverse(Node<T> last, Node<T> current) {
    if (current.getValue() == null) {
      head = last;
      return;
    }else{
      Node<T> oldNext = current.getNext();
      current.setNext(last);
      reverse(current, oldNext);
    }
  }
  
  public void reverse() {
    Node<T> empty = new Node<T>(null);
    reverse(empty, head);
  }
  
  public void print() {
    Node<T> current = head;
    while (current.getValue() != null) {
      System.out.print(current.getValue() + " ");
      current = current.getNext();
    }
    System.out.println();
  }
}

class Node<T> {
  private T value;
  private Node next;
  
  public Node(T val) {
    value = val;
    next = null;
  }
  
  public T getValue() {
    return value;
  }
  
  public void setValue(T val) {
    value = val;
  }
  
  public Node getNext() {
    return next;
  }
  
  public void setNext(Node<T> newNext) {
    next = newNext;
  }
}
