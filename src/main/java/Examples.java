

class Summable<X extends Object> extends Object {
    X plus(X other) { return other; }
}

class Nat extends Summable<Nat> {
    Nat() { super(); }
    Succ succ() { return new Succ(this); }
}

class Zero extends Nat {
    Zero() { super(); }
    public String toString() { return "Zero"; }
}

class Succ extends Nat {
    Nat prev;
    Succ(Nat prev) { super(); this.prev = prev; }
    Nat plus(Nat n) {
        return this.prev.plus(n.succ());
    }
    public String toString() { return "Succ(" + prev.toString() + ")"; }
}

class UnaryFunc<X extends Object, Y extends Object> extends Object {
    Y ignored;
    UnaryFunc(Y ignored) { super(); this.ignored = ignored; }
    Y apply(X arg) {
        return this.ignored;
    }
}

class TwicePlus1 extends UnaryFunc<Nat, Nat> {
    TwicePlus1(Nat ignored) { super(ignored); }

    Succ apply(Nat n) { return n.plus(n).succ(); }
}


class List<X extends Summable<X>> extends Object {
    List() { super(); }
    X sum(X zero) { return zero; }
    <Y extends Summable<Y>> List<Y> map(UnaryFunc<X, Y> f) {
        return new Nil<Y>();
    }
}

class Nil<X extends Summable<X>> extends List<X> {
    Nil() { super(); }
    public String toString() { return "Nil"; }
}

class Cons<X extends Summable<X>> extends List<X> {
    X head;
    List<X> tail;

    Cons(X head, List<X> tail) { super(); this.head = head; this.tail = tail; }

    X sum(X zero) { return this.tail.sum(zero).plus(head); }

    <Y extends Summable<Y>> List<Y> map(UnaryFunc<X, Y> f) {
        return new Cons<Y>(f.apply(this.head), this.tail.map(f));
    }

    public String toString() { return head.toString() + " :: " + tail.toString(); }
}



class Examples {

    public static void main(String[] args) {
        Nat zero = new Zero();
        Nat one = new Succ(zero);
        Nat two = new Succ(one);
        Nat three = new Succ(two);

        List<Nat> empty = new Nil<Nat>();
        List<Nat> two_ = new Cons<Nat>(two, empty);
        List<Nat> three_two_ = new Cons<Nat>(three, two_);

        TwicePlus1 twicePlus1 = new TwicePlus1(zero);


        System.out.println(one.plus(three));
        System.out.println(three.plus(three));

        System.out.println(three_two_);
        System.out.println(three_two_.sum(zero));

        System.out.println(three_two_.map(twicePlus1));
        System.out.println(three_two_.map(twicePlus1).sum(new Zero()));


    }
}