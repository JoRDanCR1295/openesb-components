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
 * @(#)CollectionNodeIterator.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath.ri.model.beans;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.jxpath.JXPathException;
import org.apache.commons.jxpath.ri.model.NodeIterator;
import org.apache.commons.jxpath.ri.model.NodePointer;

/**
 * Combines node iterators of all elements of a collection into one
 * aggregate node iterator.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public abstract class CollectionNodeIterator implements NodeIterator {
    private CollectionPointer pointer;
    private boolean reverse;
    private NodePointer startWith; 
    private int position;
    private List collection;

    protected CollectionNodeIterator(
        CollectionPointer pointer,
        boolean reverse,
        NodePointer startWith) 
    {
        this.pointer = pointer;
        this.reverse = reverse;
        this.startWith = startWith;
    }
    
    /**
     * Implemened by subclasses to produce child/attribute node iterators.
     */
    protected abstract NodeIterator 
            getElementNodeIterator(NodePointer elementPointer);

    public int getPosition() {
        return position;
    }

    public boolean setPosition(int position) {
        if (collection == null) {
            prepare();
        }
        
        if (position < 1 || position > collection.size()) {
            return false;
        }
        this.position = position;
        return true;
    }

    public NodePointer getNodePointer() {
        if (position == 0) {
            return null;
        }
        return (NodePointer) collection.get(position - 1);
    }
    
    private void prepare() {
        collection = new ArrayList();
        NodePointer ptr = (NodePointer) pointer.clone();
        int length = ptr.getLength();
        for (int i = 0; i < length; i++) {
            ptr.setIndex(i);
            NodePointer elementPointer = ptr.getValuePointer();
            NodeIterator iter = getElementNodeIterator(elementPointer);

            for (int j = 1; iter.setPosition(j); j++) {
                NodePointer childPointer = iter.getNodePointer();
                if (reverse) {
                    collection.add(0, childPointer);
                }
                else {
                    collection.add(childPointer);
                }
            }
        }
        if (startWith != null) {
            int index = collection.indexOf(startWith);
            if (index == -1) {
                throw new JXPathException(
                    "Invalid starting pointer for iterator: " + startWith);
            }
            while (collection.size() > index) {
                if (!reverse) {
                    collection.remove(collection.size() - 1);
                }
                else {
                    collection.remove(0);
                }
            }
        }
    }
}
