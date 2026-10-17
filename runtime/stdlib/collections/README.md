# Collections

`std.collections.list` exports the mutable `List<T>` interface.
`std.collections.linked_list` exports `LinkedList<T>`, a circular doubly linked
implementation. Class references are pointers, so node links need no separate
pointer type or dereference syntax.

```blink
import std.collections.list as lists;
import std.collections.linked_list as linked;

fun first<T>(items: lists.List<T>) => T { return items.get(0); }

fun main() => i32 {
    let items = new linked.LinkedList<i32> {};
    items.add(20);
    items.addLast(22);
    let view: lists.List<i32> = items;
    let result = first(items) + view.get(-1);
    view.clear();
    free items;
    return result;
}
```

Pass `-stdlib-root runtime/stdlib` when compiling from the repository root.
`new linked.LinkedList<T> {}` creates an empty list. Ordinary inference keeps
its concrete type; generic functions taking `List<T>` can infer `T` from the
implemented interface, as in `first(items)`.

## List operations

Indices are signed `i64` values. Nonnegative indices start at zero; negative
indices count from the end: `-1` selects the last element, `-2` the
second-to-last, and `-size()` the first. For `get`, `set`, and `removeAt`, valid
indices satisfy `-size() <= index < size()` on a nonempty list. Negative
indices normalize to `size() + index` before the existing walk from the nearer
end. They do not wrap around the circular list repeatedly.

Insertion uses the same positions and inserts before the selected element:
`insert(-1, value)` inserts before the current last element, and
`insert(-size(), value)` prepends. The additional position `index == size()`
appends. On an empty list, only `insert(0, value)` is valid. Indices outside
these bounds and reading or removing an end of an empty list call `exit(1)`.

| Method | Behavior | LinkedList cost |
| --- | --- | --- |
| `size() => i64` | Number of elements | O(1) |
| `isEmpty() => bool` | Whether the list has no elements | O(1) |
| `get(index) => T` | Read an element | O(1 + min(position, size - 1 - position)) |
| `set(index, value) => T` | Replace an element and return the old value | Same as `get` |
| `add(value)` | Append an element | O(1) |
| `insert(index, value)` | Insert before an index, shifting later indices | O(1) at either end; O(n) otherwise |
| `removeAt(index) => T` | Unlink an element and return its value | Same as `get` |
| `clear()` | Free all nodes and leave the list empty and reusable | O(n) |

Here `position` is the normalized nonnegative index.

`LinkedList<T>` additionally provides `addFirst`, `addLast`, `getFirst`,
`getLast`, `removeFirst`, and `removeLast`, all O(1). The getters and removers
return `T`; additions return void. `List<T>` specifies only the common indexed
sequence operations, so other implementations can supply their own storage.

## Storage and ownership

The empty list has a null head. A nonempty list is circular: the tail's `next`
is the head, the head's `prev` is the tail, and a singleton points to itself in
both directions. Indexed access walks from the nearer end. Nodes are internal
to the module. Blink has no private class members yet; the list's storage and
helper methods are implementation details and should not be mutated or called
by users.

The list owns its nodes. Stored object, interface, and array references remain
caller-owned: removing, replacing, or clearing an element does not free its
payload. Referenced values must stay alive while the list uses them. Heap
arrays (`new [...]`) are appropriate when an array must outlive its creating
function. Scalar values are stored by value. Direct function-valued elements
remain subject to Blink's existing restriction on function-typed class fields;
use class or interface values exposing methods to store callable behavior.

Before freeing a populated list, call `clear()` to release its nodes, then
`free` the list object once. Clearing through a `List<T>` view works too.
Freeing an object is shallow in Blink; `free items` alone does not traverse the
ring. Free stored objects separately after all references to them are finished.
