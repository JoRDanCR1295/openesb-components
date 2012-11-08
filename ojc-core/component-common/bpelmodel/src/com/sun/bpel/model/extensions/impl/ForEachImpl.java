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
 * @(#)ForEachImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model.extensions.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import javax.xml.namespace.QName;

import com.sun.bpel.model.Assign;
import com.sun.bpel.model.BPELHelper;
import com.sun.bpel.model.Variable;
import com.sun.bpel.model.Copy;
import com.sun.bpel.model.PartnerLink;
import com.sun.bpel.model.extensions.Choose;
import com.sun.bpel.model.extensions.ForEach;
import com.sun.bpel.model.extensions.ForEach.ATTR;
import com.sun.bpel.model.impl.BPELElementImpl;
import com.sun.bpel.model.visitor.BPELVisitor;
import com.sun.bpel.model.wsdlmodel.impl.XMLAttributeImpl;
import com.sun.bpel.xml.common.model.XMLAttribute;
import com.sun.bpel.xml.common.model.XMLDocument;
import com.sun.bpel.xml.common.model.XMLElement;
import com.sun.bpel.xml.common.model.XMLNode;
import com.sun.bpel.xml.common.visitor.Visitor;

/**
 * Implements the &lt;forEach&gt; element.
 *
 * @author Sun Microsystems
 */
public class ForEachImpl extends BPELElementImpl implements ForEach {

    /** serialVersionUID for this class */
    static final long serialVersionUID = 4492531035037826454L;

    /** Holds the copy element list. */
    private ArrayList copies = new ArrayList();

    /** Holds the forEach element list. */
    private ArrayList forEachs = new ArrayList();

     /** Holds the chooses element list. */
    private ArrayList mChooses = new ArrayList();

    private PartnerLink mPartnerLink;
    
    private Variable mVariable;
    
    /** QName object for SeeBeyond Private extension line label */
    public static final QName FOREACH_QNAME =
    	com.sun.bpel.xml.NamespaceUtility.getQName(
    				XMLElement.SBYNBPEL_EXTN_NAMESPACE,
    				ForEach.TAG,
    				XMLElement.SBYNBPEL_EXTN_PREFIX);
    
    /** Creates a new instance of ForEachImpl */
    public ForEachImpl() {
        super();
        initForEach();
    }

    /** Creates a new instance of ForEachImpl.
     * @param   d   Owner document.
     */
    public ForEachImpl(XMLDocument d) {
        super(d);
        initForEach();
    }

    /** Initializes this class.
     */
    private void initForEach() {
        owningNamespace = XMLElement.SBYNBPEL_EXTN_NAMESPACE;
        owningNamespacePrefix = XMLElement.SBYNBPEL_EXTN_PREFIX;
        
        //setLocalName(ForEach.TAG);
        setQualifiedName(FOREACH_QNAME);
        xmlAttrs = new XMLAttribute[] {
            new XMLAttributeImpl(ATTR.CONTAINER, String.class, true, null),
            new XMLAttributeImpl(ATTR.PART, String.class, true, null),
            new XMLAttributeImpl(ATTR.QUERY, String.class, true, null),
            new XMLAttributeImpl(ATTR.EXPRESSION, String.class, true, null),
            new XMLAttributeImpl(ATTR.BEGIN, String.class, true, null),
            new XMLAttributeImpl(ATTR.END, String.class, true, null),
            new XMLAttributeImpl(ATTR.STEP, String.class, true, null),
        };
        childrenTags = new String[] {
            ForEach.TAG,
            Copy.TAG,
            Choose.TAG
        };
    }

    /** Getter for container attribute.
     * @return  container attribute.
     */
    public String getContainer() {
        return xmlAttrs[CONTAINER].getValue();
    }

    /** Setter for container attribute.
     * @param   c   container attribute.
     */
    public void setContainer(String variable) {
    	String oldVariable = getContainer();
    	setAttribute(CONTAINER, variable);
        //if variable is changed we need to clear out cached variable object
        //so that next call to getBPELVariable can find new variable object
        if(variable != null && !variable.equals(oldVariable)) {
        	this.mVariable = null;
        }
    }

    /** Getter for part attribute.
     * @return  part attribute.
     */
    public String getPart() {
        return xmlAttrs[PART].getValue();
    }

    /** Setter for part attribute.
     * @param   p   part attribute.
     */
    public void setPart(String p) {
        setAttribute(PART, p);
    }

    /** Getter for query attribute.
     * @return  query attribute.
     */
    public String getQuery() {
        return xmlAttrs[QUERY].getValue();
    }

    /** Setter for query attribute.
     * @param   q   query attribute.
     */
    public void setQuery(String q) {
        setAttribute(QUERY, q);
    }

    /** Getter for expression attribute.
     * @return  expression attribute.
     */
    public String getExpression() {
        return xmlAttrs[EXPRESSION].getValue();
    }

    /** Setter for expression attribute.
     * @param   e   expression attribute.
     */
    public void setExpression(String e) {
        setAttribute(EXPRESSION, e);
    }

    /** Getter for begin attribute.
     * @return  begin attribute.
     */
    public String getBegin() {
        return xmlAttrs[BEGIN].getValue();
    }

    /** Setter for begin attribute.
     * @param   b   begin attribute.
     */
    public void setBegin(String b) {
        setAttribute(BEGIN, b);
    }

    /** Getter for end attribute.
     * @return  end attribute.
     */
    public String getEnd() {
        return xmlAttrs[END].getValue();
    }

    /** Setter for end attribute.
     * @param   e   end attribute.
     */
    public void setEnd(String e) {
        setAttribute(END, e);
    }

    /** Getter for step attribute.
     * @return  step attribute.
     */
    public String getStep() {
        return xmlAttrs[STEP].getValue();
    }

    /** Setter for step attribute.
     * @param   s   step attribute.
     */
    public void setStep(String s) {
        setAttribute(STEP, s);
    }

    /** @see XMLNode#addChild(XMLNode)
     */
    public void addChild(XMLNode c) {
        if (c instanceof Copy) {
            addCopy((Copy) c);
        } else if (c instanceof ForEach) {
            addForEach((ForEach) c);
        } else if (c instanceof Choose) {
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
        } else if (c instanceof ForEach) {
            removeForEach((ForEach) c);
        } else if (c instanceof Choose) {
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
     * @return a <code>Copy</code> value
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
            replaceChild(0, (Copy) copies.get(i), c);
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
            replaceChild(0, (ForEach) forEachs.get(i), f);
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
        super.addChild(0, c);
        copies.add(c);
    }

    /**
     * Describe <code>addForEach</code> method here.
     *
     * @param f a <code>ForEach</code> value
     */
    public synchronized void addForEach(ForEach f) {
        super.addChild(0, f);
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

    
    public void setBPELVariable(Variable variable) {
		//update the variable attribute value
		if(variable != null) {
			setContainer(variable.getName());
		} else {
			setContainer(null);
		}
		
		this.mVariable = variable;
		
	}

    public Variable getBPELVariable() {
    	if(this.mVariable != null) {
			return this.mVariable;
		}
		
		if (BPELHelper.isValueAbsent(getContainer())) {
			return null;
		}
		
		this.mVariable = BPELHelper.getMatchingVariable(getContainer(), this);
		return this.mVariable;
	}


	/**
     * Check if two ForEaches have the same rules
     * @param other - another <code>ForEach</code> instance
     * @return boolean - true if same
     */
    public boolean hasSameRules(ForEach other) {
        if (other == null) {
            return false;
        }
        if (!strEqual(this.getContainer(), other.getContainer())) {
            return false;
        }
        if (!strEqual(this.getPart(), other.getPart())) {
            return false;
        }
        if (!strEqual(this.getQuery(), other.getQuery())) {
            return false;
        }
        if (!strEqual(this.getExpression(), other.getExpression())) {
            return false;
        }
        return true;
    }
    private boolean strEqual(String str1, String str2) {
        if (str1 == null) {
            if (str2 != null) {
                return false;
            }
        } else {
            if (str2 == null) {
                return false;
            } else if (!str1.equals(str2)) {
                return false;
            }
        }
        return true;
    }
    /** Accepts a visitor to perform some work on the element.
     * @param   w   The working visitor
     * @return  <tt>true</tt> if traversal is to continue.
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
}
