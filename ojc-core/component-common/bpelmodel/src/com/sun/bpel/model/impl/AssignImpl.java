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
 * @(#)AssignImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;

import com.sun.bpel.model.Assign;
import com.sun.bpel.model.Copy;
import com.sun.bpel.model.ExtensionAssignOperation;
import com.sun.bpel.model.extensions.Choose;
import com.sun.bpel.model.extensions.ForEach;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements the &lt;assign&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public class AssignImpl extends ActivityImpl implements Assign {
    
    /** serialVersionUID for this class */
    static final long serialVersionUID = -6872032980020371555L;
    
    /** Holds the copy element list. */
    private ArrayList copies = new ArrayList();
    
    /** Holds the forEach element list. */
    private ArrayList forEachs = new ArrayList();

    /** Holds the chooses element list. */
    private ArrayList mChooses = new ArrayList();
    
    private ExtensionAssignOperation extAssignOperation;


    /** Creates a new instance of AssignImpl */
    public AssignImpl() {
        super();
        initAssign();
    }
    
    /** Creates a new instance of AssignImpl.
     * @param   d   Owner document.
     */
    public AssignImpl(XMLDocument d) {
        super(d);
        initAssign();
    }
    
    /** Initialize this class. */
    private void initAssign() {
        setLocalName(Assign.TAG);
        xmlAttrs = new XMLAttribute[NUM_ATTRS];
        // NUM_STANDARD_ATTRS is equal to standardXmlAttrs.length.
        for (int i = 0; i < NUM_STANDARD_ATTRS; i++) {
        	xmlAttrs[i] = standardXmlAttrs[i];
        }
        xmlAttrs[VALIDATE] = new XMLAttributeImpl(Assign.ATTR.VALIDATE, String.class, true, 
        		XMLAttribute.BOOLEAN_ENUM_VALS);
        childrenTags = new String[] {
            Copy.TAG,
            ForEach.TAG,
            Choose.TAG
        };
    }
    
    /** @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof Copy) {
            addCopy((Copy) c);
        } if (c instanceof ForEach) {
            addForEach((ForEach) c);
        } if (c instanceof Choose) {
            addChoose((Choose) c);
        } else {
            super.addChild(c);
        }
    }
    
    /** @see XMLNode#removeChild
     */
    public void removeChild(XMLNode c) {
        if (c instanceof Copy) {
            removeCopy((Copy) c);
        } if (c instanceof ForEach) {
            removeForEach((ForEach) c);
        } if (c instanceof Choose) {
            removeChoose((Choose) c);
        } else {
            super.removeChild(c);
        }
    }
    
    /** @see XMLNode#getCopy
     */
    public Copy getCopy(int i) {
        return (Copy) copies.get(i);
    }
    
    /**
     * Describe <code>getForEach</code> method here.
     *
     * @param i an <code>int</code> value
     * @return a <code>ForEach</code> value
     */
    public ForEach getForEach(int i) {
        return (ForEach) forEachs.get(i);
    }

    /**
     * Describe <code>getChoose</code> method here.
     *
     * @param i an <code>int</code> value
     * @return a <code>Copy</code> value
     */
    public Choose getChoose(int i) {
        return (Choose) mChooses.get(i);
    }

    /** @see XMLNode#setCopy
     */
    public synchronized void setCopy(int i, Copy c) {
        if (copies.size() == i) {
            addCopy(c);
        } else {
            replaceChild(3, (Copy) copies.get(i), c);
            copies.set(i, c);
        }
    }
    
    /**
     * Describe <code>setForEach</code> method here.
     *
     * @param i an <code>int</code> value
     * @param f a <code>ForEach</code> value
     */
    public synchronized void setForEach(int i, ForEach f) {
        if (forEachs.size() == i) {
            addForEach(f);
        } else {
            replaceChild(3, (ForEach) forEachs.get(i), f);
            forEachs.set(i, f);
        }
    }

    /**
     * Describe <code>setChoose</code> method here.
     *
     * @param i an <code>int</code> value
     * @param c a <code>Choose</code> value
     */
    public synchronized void setChoose(int i, Choose c) {
        if (mChooses.size() == i) {
            addChoose(c);
        } else {
            replaceChild(3, (Choose) mChooses.get(i), c);
            mChooses.set(i, c);
        }
    }

    /** @see XMLNode#addCopy(Copy)
     */
    public synchronized void addCopy(Copy c) {
        super.addChild(3, c);
        copies.add(c);
    }
    
    /**
     * Describe <code>addForEach</code> method here.
     *
     * @param f a <code>ForEach</code> value
     */
    public synchronized void addForEach(ForEach f) {
        super.addChild(3, f);
        forEachs.add(f);
    }

    /**
     * Describe <code>addChoose</code> method here.
     *
     * @param c a <code>Choose</code> value
     */
    public synchronized void addChoose(Choose c) {
        super.addChild(3, c);
        mChooses.add(c);
    }

    /** @see Assign#addCopy(int, Copy)
     */
    public synchronized void addCopy(int index, Copy c) {
        if ((index < 0) || (getChildren() != null && index > getChildren().size())) {
            throw new ArrayIndexOutOfBoundsException("Expected: 0 <= index <= " + getChildren().size());
        } else if ((index == 0 && getChildren() == null) || 
            (getChildren() != null && index == getChildren().size())) {
            super.addChild(3, c);
            addCopy(c);
        } else {
            super.addChild(3, (XMLNode) getChildren().get(index), c);
            copies.clear();
            for (Iterator iter = getChildren().iterator(); iter.hasNext();) {
                XMLNode kid = (XMLNode) iter.next();
                if (kid instanceof Copy) {
                    copies.add(kid);
                }
            }
        }
    }
    
    /**
     * Describe <code>addForEach</code> method here.
     *
     * @param index an <code>int</code> value
     * @param f a <code>ForEach</code> value
     */
    public synchronized void addForEach(int index, ForEach f) {
        if ((index < 0) || (getChildren() != null && index > getChildren().size())) {
            throw new ArrayIndexOutOfBoundsException("Expected: 0 <= index <= " + getChildren().size());
        } else if ((index == 0 && getChildren() == null) || 
            (getChildren() != null && index == getChildren().size())) {
            super.addChild(3, f);
            addForEach(f);
        } else {
            super.addChild(3, (XMLNode) getChildren().get(index), f);
            forEachs.clear();
            for (Iterator iter = getChildren().iterator(); iter.hasNext();) {
                XMLNode kid = (XMLNode) iter.next();
                if (kid instanceof ForEach) {
                    forEachs.add(kid);
                }
            }
        }
    }

    /**
     * Describe <code>addChoose</code> method here.
     *
     * @param index an <code>int</code> value
     * @param c a <code>Choose</code> value
     */
     public synchronized void addChoose(int index, Choose c) {
        if ((index < 0) || (getChildren() != null && index > getChildren().size())) {
            throw new ArrayIndexOutOfBoundsException("Expected: 0 <= index <= " + getChildren().size());
        } else if ((index == 0 && getChildren() == null) || 
            (getChildren() != null && index == getChildren().size())) {
            super.addChild(3, c);
            addChoose(c);
        } else {
            super.addChild(3, (XMLNode) getChildren().get(index), c);
            mChooses.clear();
            for (Iterator iter = getChildren().iterator(); iter.hasNext();) {
                XMLNode kid = (XMLNode) iter.next();
                if (kid instanceof Choose) {
                    mChooses.add(kid);
                }
            }
        }
    }


    /** @see Assign#clearCopies
     */
    public synchronized void clearCopies() {
        while (copies.size() > 0) {
            removeCopy(0);  // stays at 0 because array elements keep shifting to the left
        }
    }
    
    /**
     * Describe <code>clearForEachs</code> method here.
     *
     */
    public synchronized void clearForEachs() {
        while (forEachs.size() > 0) {
            removeForEach(0);  // stays at 0 because array elements keep shifting to the left
        }
    }

    /**
     * Describe <code>clearChooses</code> method here.
     *
     */
    public synchronized void clearChooses() {
        while (mChooses.size() > 0) {
            removeChoose(0);  // stays at 0 because array elements keep shifting to the left
        }
    }

    /** @see Assign#removeCopy(int)
     */
    public synchronized void removeCopy(int i) {
        removeCopy(getCopy(i));
    }
    
    /**
     * Describe <code>removeForEach</code> method here.
     *
     * @param i an <code>int</code> value
     */
    public synchronized void removeForEach(int i) {
        removeForEach(getForEach(i));
    }

    /**
     * Describe <code>removeChoose</code> method here.
     *
     * @param i an <code>int</code> value
     */
    public synchronized void removeChoose(int i) {
        removeChoose(getChoose(i));
    }


    /** @see Assign#removeCopy(Copy)
     */
    public synchronized boolean removeCopy(Copy c) {
        super.removeChild(c);
        return copies.remove(c);
    }
    
    /**
     * Describe <code>removeForEach</code> method here.
     *
     * @param f a <code>ForEach</code> value
     * @return a <code>boolean</code> value
     */
    public synchronized boolean removeForEach(ForEach f) {
        super.removeChild(f);
        return forEachs.remove(f);
    }

    /**
     * Describe <code>removeChoose</code> method here.
     *
     * @param f a <code>Choose</code> value
     * @return a <code>boolean</code> value
     */
    public synchronized boolean removeChoose(Choose f) {
        super.removeChild(f);
        return mChooses.remove(f);
    }
    
    /** @see Assign#getCopySize
     */
    public int getCopySize() {
        return copies.size();
    }
    
    /**
     * Describe <code>getForEachSize</code> method here.
     *
     * @return an <code>int</code> value
     */
    public int getForEachSize() {
        return forEachs.size();
    }

    /**
     * Describe <code>getChooseSize</code> method here.
     *
     * @return an <code>int</code> value
     */
    public int getChooseSize() {
        return mChooses.size();
    }
    
    /** @see Assign#indexOfCopy
     */
    public int indexOfCopy(XMLNode copy) {
        return copies.indexOf(copy);
    }
    
    /**
     * Describe <code>indexOfForEach</code> method here.
     *
     * @param forEach a <code>XMLNode</code> value
     * @return an <code>int</code> value
     */
    public int indexOfForEach(XMLNode forEach) {
        return forEachs.indexOf(forEach);
    }

    /**
     * Describe <code>indexOfChoose</code> method here.
     *
     * @param choose a <code>XMLNode</code> value
     * @return an <code>int</code> value
     */
    public int indexOfChoose(XMLNode choose) {
        return mChooses.indexOf(choose);
    }

    /** @see Assign#getCopies
     */
    public synchronized Collection getCopies() {
        return Collections.unmodifiableCollection((ArrayList) copies.clone());
    }
    
    /**
     * Describe <code>getForEachs</code> method here.
     *
     * @return a <code>Collection</code> value
     */
    public synchronized Collection getForEachs() {
        return Collections.unmodifiableCollection((ArrayList) forEachs.clone());
    }

    /**
     * Describe <code>getChooses</code> method here.
     *
     * @return a <code>Collection</code> value
     */
    public synchronized Collection getChooses() {
        return Collections.unmodifiableCollection((ArrayList) mChooses.clone());
    }

    /** @see XMLNode#accept
     */
    public boolean accept(Visitor w) {
        BPELVisitor v = morphVisitor(w);
        if (traverseParentFirst(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }
        
        if (!super.accept(v)) {
            return false;
        }
        
        if (traverseParentLast(v)) {
            if (!v.visit(this)) {
                return false;
            }
        }
        return true;
    }

    public ExtensionAssignOperation getExtensionAssignOperation() {
        return extAssignOperation;
    }

    public void setExtensionAssignOperation(ExtensionAssignOperation e) {
        extAssignOperation = e;
    }

    public String getValidate() {
        return xmlAttrs[VALIDATE].getValue();
    }

    public void setValidate(String validate) {
        setAttribute(VALIDATE, validate);
    }
}
