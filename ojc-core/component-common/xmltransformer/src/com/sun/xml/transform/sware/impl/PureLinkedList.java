/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)PureLinkedList.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.impl;

import java.util.AbstractSequentialList;
import java.util.Collection;
import java.util.ConcurrentModificationException;
import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;

/**
 * Copied from JDK 1.5 LinkedList and modified a bit (exposed class 
 * <code>Entry</code>, method the <code>addBefore()</code> and added
 * method <code>addAfter</code> and method <code>addFirstEntry</code> etc.)
 *  
 * @param <E> Any entry class
 */
class PureLinkedList<E> extends AbstractSequentialList<E> {
    private transient Entry<E> header = new Entry<E>(null, null, null);
    private transient int size = 0;
    
    /**
     * Constructs an empty list.
     */
    PureLinkedList() {
        header.next = header.previous = header;
    }
    
    /**
     * Constructs a list containing the elements of the specified
     * collection, in the order they are returned by the collection's
     * iterator.
     *
     * @param  c the collection whose elements are to be placed into this list.
     * @throws NullPointerException if the specified collection is null.
     */
     PureLinkedList(Collection<? extends E> c) {
         this();
         addAll(c);
     }
    
    /**
     * Returns the first element in this list.
     *
     * @return the first element in this list.
     * @throws    NoSuchElementException if this list is empty.
     */
    public E getFirst() {
        if (size==0)
            throw new NoSuchElementException();
        
        return header.next.element;
    }
    
    /**
     * Returns the last element in this list.
     *
     * @return the last element in this list.
     * @throws    NoSuchElementException if this list is empty.
     */
    public E getLast()  {
        if (size==0)
            throw new NoSuchElementException();
        
        return header.previous.element;
    }
    
    /**
     * Removes and returns the first element from this list.
     *
     * @return the first element from this list.
     * @throws    NoSuchElementException if this list is empty.
     */
    public E removeFirst() {
        return remove(header.next);
    }
    
    /**
     * Removes and returns the last element from this list.
     *
     * @return the last element from this list.
     * @throws    NoSuchElementException if this list is empty.
     */
    public E removeLast() {
        return remove(header.previous);
    }
    
    /**
     * Inserts the given element at the beginning of this list.
     *
     * @param o the element to be inserted at the beginning of this list.
     */
    public void addFirst(E o) {
        addBefore(o, header.next);
    }
    
    /**
     * Appends the given element to the end of this list.  (Identical in
     * function to the <tt>add</tt> method; included only for consistency.)
     *
     * @param o the element to be inserted at the end of this list.
     */
    public void addLast(E o) {
        addBefore(o, header);
    }
    
    /**
     * Returns <tt>true</tt> if this list contains the specified element.
     * More formally, returns <tt>true</tt> if and only if this list contains
     * at least one element <tt>e</tt> such that <tt>(o==null ? e==null
     * : o.equals(e))</tt>.
     *
     * @param o element whose presence in this list is to be tested.
     * @return <tt>true</tt> if this list contains the specified element.
     */
    public boolean contains(Object o) {
        return indexOf(o) != -1;
    }
    
    /**
     * Returns the number of elements in this list.
     *
     * @return the number of elements in this list.
     */
    public int size() {
        return size;
    }
    
    /**
     * Appends the specified element to the end of this list.
     *
     * @param o element to be appended to this list.
     * @return <tt>true</tt> (as per the general contract of
     * <tt>Collection.add</tt>).
     */
    public boolean add(E o) {
        addBefore(o, header);
        return true;
    }
    
    /**
     * Removes the first occurrence of the specified element in this list.  If
     * the list does not contain the element, it is unchanged.  More formally,
     * removes the element with the lowest index <tt>i</tt> such that
     * <tt>(o==null ? get(i)==null : o.equals(get(i)))</tt> (if such an
     * element exists).
     *
     * @param o element to be removed from this list, if present.
     * @return <tt>true</tt> if the list contained the specified element.
     */
    public boolean remove(Object o) {
        if (o==null) {
            for (Entry<E> e = header.next; e != header; e = e.next) {
                if (e.element==null) {
                    remove(e);
                    return true;
                }
            }
        } else {
            for (Entry<E> e = header.next; e != header; e = e.next) {
                if (o.equals(e.element)) {
                    remove(e);
                    return true;
                }
            }
        }
        return false;
    }
    
    /**
     * Appends all of the elements in the specified collection to the end of
     * this list, in the order that they are returned by the specified
     * collection's iterator.  The behavior of this operation is undefined if
     * the specified collection is modified while the operation is in
     * progress.  (This implies that the behavior of this call is undefined if
     * the specified Collection is this list, and this list is nonempty.)
     *
     * @param c the elements to be inserted into this list.
     * @return <tt>true</tt> if this list changed as a result of the call.
     * @throws NullPointerException if the specified collection is null.
     */
    public boolean addAll(Collection<? extends E> c) {
        return addAll(size, c);
    }
    
    /**
     * Inserts all of the elements in the specified collection into this
     * list, starting at the specified position.  Shifts the element
     * currently at that position (if any) and any subsequent elements to
     * the right (increases their indices).  The new elements will appear
     * in the list in the order that they are returned by the
     * specified collection's iterator.
     *
     * @param index index at which to insert first element
     *          from the specified collection.
     * @param c elements to be inserted into this list.
     * @return <tt>true</tt> if this list changed as a result of the call.
     * @throws IndexOutOfBoundsException if the specified index is out of
     *            range (<tt>index &lt; 0 || index &gt; size()</tt>).
     * @throws NullPointerException if the specified collection is null.
     */
    public boolean addAll(int index, Collection<? extends E> c) {
        if (index < 0 || index > size)
            throw new IndexOutOfBoundsException("Index: "+index+
                                                ", Size: "+size);
        Object[] a = c.toArray();
        int numNew = a.length;
        if (numNew==0)
            return false;
        modCount++;
    
        Entry<E> successor = (index==size ? header : entry(index));
        Entry<E> predecessor = successor.previous;
        for (int i=0; i<numNew; i++) {
            Entry<E> e = new Entry<E>((E)a[i], successor, predecessor);
            predecessor.next = e;
            predecessor = e;
        }
        successor.previous = predecessor;
    
        size += numNew;
        return true;
    }
    
    /**
     * Removes all of the elements from this list.
     */
    public void clear() {
        Entry<E> e = header.next;
        while (e != header) {
            Entry<E> next = e.next;
            e.next = e.previous = null;
            e.element = null;
            e = next;
        }
        header.next = header.previous = header;
        size = 0;
        modCount++;
    }
    
    
    // Positional Access Operations
    
    /**
     * Returns the element at the specified position in this list.
     *
     * @param index index of element to return.
     * @return the element at the specified position in this list.
     *
     * @throws IndexOutOfBoundsException if the specified index is out of
     * range (<tt>index &lt; 0 || index &gt;= size()</tt>).
     */
    public E get(int index) {
        return entry(index).element;
    }
    
    /**
     * Replaces the element at the specified position in this list with the
     * specified element.
     *
     * @param index index of element to replace.
     * @param element element to be stored at the specified position.
     * @return the element previously at the specified position.
     * @throws IndexOutOfBoundsException if the specified index is out of
     *        range (<tt>index &lt; 0 || index &gt;= size()</tt>).
     */
    public E set(int index, E element) {
        Entry<E> e = entry(index);
        E oldVal = e.element;
        e.element = element;
        return oldVal;
    }
    
    /**
     * Inserts the specified element at the specified position in this list.
     * Shifts the element currently at that position (if any) and any
     * subsequent elements to the right (adds one to their indices).
     *
     * @param index index at which the specified element is to be inserted.
     * @param element element to be inserted.
     *
     * @throws IndexOutOfBoundsException if the specified index is out of
     *        range (<tt>index &lt; 0 || index &gt; size()</tt>).
     */
    public void add(int index, E element) {
        addBefore(element, (index==size ? header : entry(index)));
    }
    
    /**
     * Removes the element at the specified position in this list.  Shifts any
     * subsequent elements to the left (subtracts one from their indices).
     * Returns the element that was removed from the list.
     *
     * @param index the index of the element to removed.
     * @return the element previously at the specified position.
     *
     * @throws IndexOutOfBoundsException if the specified index is out of
     *        range (<tt>index &lt; 0 || index &gt;= size()</tt>).
     */
    public E remove(int index) {
        return remove(entry(index));
    }
    
    /**
     * Return the indexed entry.
     */
    private Entry<E> entry(int index) {
        if (index < 0 || index >= size)
            throw new IndexOutOfBoundsException("Index: "+index+
                                                ", Size: "+size);
        Entry<E> e = header;
        if (index < (size >> 1)) {
            for (int i = 0; i <= index; i++)
                e = e.next;
        } else {
            for (int i = size; i > index; i--)
                e = e.previous;
        }
        return e;
    }
    
    
    // Search Operations
    
    /**
     * Returns the index in this list of the first occurrence of the
     * specified element, or -1 if the List does not contain this
     * element.  More formally, returns the lowest index i such that
     * <tt>(o==null ? get(i)==null : o.equals(get(i)))</tt>, or -1 if
     * there is no such index.
     *
     * @param o element to search for.
     * @return the index in this list of the first occurrence of the
     *         specified element, or -1 if the list does not contain this
     *         element.
     */
    public int indexOf(Object o) {
        int index = 0;
        if (o==null) {
            for (Entry e = header.next; e != header; e = e.next) {
                if (e.element==null)
                    return index;
                index++;
            }
        } else {
            for (Entry e = header.next; e != header; e = e.next) {
                if (o.equals(e.element))
                    return index;
                index++;
            }
        }
        return -1;
    }
    
    /**
     * Returns the index in this list of the last occurrence of the
     * specified element, or -1 if the list does not contain this
     * element.  More formally, returns the highest index i such that
     * <tt>(o==null ? get(i)==null : o.equals(get(i)))</tt>, or -1 if
     * there is no such index.
     *
     * @param o element to search for.
     * @return the index in this list of the last occurrence of the
     *         specified element, or -1 if the list does not contain this
     *         element.
     */
    public int lastIndexOf(Object o) {
        int index = size;
        if (o==null) {
            for (Entry e = header.previous; e != header; e = e.previous) {
                index--;
                if (e.element==null)
                    return index;
            }
        } else {
            for (Entry e = header.previous; e != header; e = e.previous) {
                index--;
                if (o.equals(e.element))
                    return index;
            }
        }
        return -1;
    }
    
    // Queue operations.
    
    /**
     * Retrieves, but does not remove, the head (first element) of this list.
     * @return the head of this queue, or <tt>null</tt> if this queue is empty.
     * @since 1.5
     */
    public E peek() {
        if (size==0)
            return null;
        return getFirst();
    }
    
    /**
     * Retrieves, but does not remove, the head (first element) of this list.
     * @return the head of this queue.
     * @throws NoSuchElementException if this queue is empty.
     * @since 1.5
     */
    public E element() {
        return getFirst();
    }
    
    /**
     * Retrieves and removes the head (first element) of this list.
     * @return the head of this queue, or <tt>null</tt> if this queue is empty.
     * @since 1.5
     */
    public E poll() {
        if (size==0)
            return null;
        return removeFirst();
    }
    
    /**
     * Retrieves and removes the head (first element) of this list.
     * @return the head of this queue.
     * @throws NoSuchElementException if this queue is empty.
     * @since 1.5
     */
    public E remove() {
        return removeFirst();
    }
    
    /**
     * Adds the specified element as the tail (last element) of this list.
     *
     * @param o the element to add.
     * @return <tt>true</tt> (as per the general contract of
     * <tt>Queue.offer</tt>)
     * @since 1.5
     */
    public boolean offer(E o) {
        return add(o);
    }
    
    /**
     * Returns a list-iterator of the elements in this list (in proper
     * sequence), starting at the specified position in the list.
     * Obeys the general contract of <tt>List.listIterator(int)</tt>.<p>
     *
     * The list-iterator is <i>fail-fast</i>: if the list is structurally
     * modified at any time after the Iterator is created, in any way except
     * through the list-iterator's own <tt>remove</tt> or <tt>add</tt>
     * methods, the list-iterator will throw a
     * <tt>ConcurrentModificationException</tt>.  Thus, in the face of
     * concurrent modification, the iterator fails quickly and cleanly, rather
     * than risking arbitrary, non-deterministic behavior at an undetermined
     * time in the future.
     *
     * @param index index of first element to be returned from the
     *          list-iterator (by a call to <tt>next</tt>).
     * @return a ListIterator of the elements in this list (in proper
     *         sequence), starting at the specified position in the list.
     * @throws    IndexOutOfBoundsException if index is out of range
     *        (<tt>index &lt; 0 || index &gt; size()</tt>).
     * @see List#listIterator(int)
     */
    public ListIterator<E> listIterator(int index) {
        return new ListItr(index);
    }
    
    private class ListItr implements ListIterator<E> {
        private Entry<E> lastReturned = header;
        private Entry<E> next;
        private int nextIndex;
        private int expectedModCount = modCount;
        
        ListItr(int index) {
            if (index < 0 || index > size)
            throw new IndexOutOfBoundsException("Index: "+index+
                                ", Size: "+size);
            if (index < (size >> 1)) {
                next = header.next;
                for (nextIndex=0; nextIndex<index; nextIndex++)
                    next = next.next;
            } else {
                next = header;
                for (nextIndex=size; nextIndex>index; nextIndex--)
                    next = next.previous;
            }
        }
        
        public boolean hasNext() {
            return nextIndex != size;
        }
        
        public E next() {
            checkForComodification();
            if (nextIndex == size)
            throw new NoSuchElementException();
        
            lastReturned = next;
            next = next.next;
            nextIndex++;
            return lastReturned.element;
        }
        
        public boolean hasPrevious() {
            return nextIndex != 0;
        }
        
        public E previous() {
            if (nextIndex == 0)
            throw new NoSuchElementException();
        
            lastReturned = next = next.previous;
            nextIndex--;
            checkForComodification();
            return lastReturned.element;
        }
        
        public int nextIndex() {
            return nextIndex;
        }
        
        public int previousIndex() {
            return nextIndex-1;
        }
        
        public void remove() {
                checkForComodification();
                Entry<E> lastNext = lastReturned.next;
                try {
                    PureLinkedList.this.remove(lastReturned);
                } catch (NoSuchElementException e) {
                    throw new IllegalStateException();
                }
            if (next==lastReturned)
                next = lastNext;
            else
                nextIndex--;
            lastReturned = header;
            expectedModCount++;
        }
        
        public void set(E o) {
            if (lastReturned == header)
            throw new IllegalStateException();
            checkForComodification();
            lastReturned.element = o;
        }
        
        public void add(E o) {
            checkForComodification();
            lastReturned = header;
            addBefore(o, next);
            nextIndex++;
            expectedModCount++;
        }
        
        final void checkForComodification() {
            if (modCount != expectedModCount)
            throw new ConcurrentModificationException();
        }
    }
    
    public static class Entry<E> {
        private E element;
        private Entry<E> next;
        private Entry<E> previous;
        
        Entry(E element, Entry<E> next, Entry<E> previous) {
            this.element = element;
            this.next = next;
            this.previous = previous;
        }
        
        public E getElement() {
            return element;
        }
    }
    
    public Entry<E> addBefore(E o, Entry<E> e) {
        Entry<E> newEntry = new Entry<E>(o, e, e.previous);
        newEntry.previous.next = newEntry;
        newEntry.next.previous = newEntry;
        size++;
        modCount++;
        return newEntry;
    }
    
    public Entry<E> addAfter(E o, Entry<E> e) {
        Entry<E> newEntry = new Entry<E>(o, e.next, e);
        newEntry.next.previous = newEntry;
        newEntry.previous.next = newEntry;
        size++;
        modCount++;
        return newEntry;
    }

    public Entry<E> addFirstEntry(E o) {
        return addBefore(o, header.next);
    }
    
    public E remove(Entry<E> e) {
        if (e == header)
            throw new NoSuchElementException();
        
        E result = e.element;
        e.previous.next = e.next;
        e.next.previous = e.previous;
        e.next = e.previous = null;
        e.element = null;
        size--;
        modCount++;
        return result;
    }
    
    /**
     * Returns an array containing all of the elements in this list
     * in the correct order.
     *
     * @return an array containing all of the elements in this list
     *         in the correct order.
     */
    public Object[] toArray() {
        Object[] result = new Object[size];
        int i = 0;
        for (Entry<E> e = header.next; e != header; e = e.next)
            result[i++] = e.element;
        return result;
    }
    
    /**
     * Returns an array containing all of the elements in this list in
     * the correct order; the runtime type of the returned array is that of
     * the specified array.  If the list fits in the specified array, it
     * is returned therein.  Otherwise, a new array is allocated with the
     * runtime type of the specified array and the size of this list.<p>
     *
     * If the list fits in the specified array with room to spare
     * (i.e., the array has more elements than the list),
     * the element in the array immediately following the end of the
     * collection is set to null.  This is useful in determining the length
     * of the list <i>only</i> if the caller knows that the list
     * does not contain any null elements.
     *
     * @param a the array into which the elements of the list are to
     *      be stored, if it is big enough; otherwise, a new array of the
     *      same runtime type is allocated for this purpose.
     * @return an array containing the elements of the list.
     * @throws ArrayStoreException if the runtime type of a is not a
     *         supertype of the runtime type of every element in this list.
     * @throws NullPointerException if the specified array is null.
     */
    public <T> T[] toArray(T[] a) {
        if (a.length < size)
            a = (T[])java.lang.reflect.Array.newInstance(
                                a.getClass().getComponentType(), size);
        int i = 0;
        Object[] result = a;
        for (Entry<E> e = header.next; e != header; e = e.next)
            result[i++] = e.element;
    
        if (a.length > size)
            a[size] = null;
    
        return a;
    }
}
