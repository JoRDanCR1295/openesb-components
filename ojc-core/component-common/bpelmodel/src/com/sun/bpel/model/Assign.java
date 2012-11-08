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
 * @(#)Assign.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.bpel.model;


import java.util.Collection;

import com.sun.bpel.model.extensions.Choose;
import com.sun.bpel.model.extensions.ForEach;
import com.sun.bpel.xml.common.model.XMLNode;

/**
 * Describes the &lt;assign&gt; element.
 *
 * @author Sun Microsystems
 * @version 
 */
public interface Assign extends Activity {
    /** Tag for this element. */
    public static final String TAG = Tags.ASSIGN;
    //attribute ordinal index
    public static final int VALIDATE = NUM_STANDARD_ATTRS;
    
    /** Total number of attributes */
    public static final int NUM_ATTRS = VALIDATE + 1;

    /** Describes the attributes of this element.
     */
    public interface ATTR {
        public static final String VALIDATE = "validate";
    }
    
    /** Indexed getter for copy elements.
     * @param   i   Index to copy element.
     * @return  copy element.
     */
    Copy getCopy(int i);
    
    /**
     * Describe <code>getForEach</code> method here.
     *
     * @param i an <code>int</code> value
     * @return a <code>ForEach</code> value
     */
    ForEach getForEach(int i);

    /**
     * Describe <code>getChoose</code> method here.
     *
     * @param i an <code>int</code> value
     * @return a <code>ForEach</code> value
     */
    Choose getChoose(int i);
    

    /** Indexed setter for copy element.
     * @param   i   Index to copy element.
     * @param   c   copy element.
     */
    void setCopy(int i, Copy c);
    
    /**
     * Describe <code>setForEach</code> method here.
     *
     * @param i an <code>int</code> value
     * @param f a <code>ForEach</code> value
     */
    void setForEach(int i, ForEach f);

    /**
     * Describe <code>setChoose</code> method here.
     *
     * @param i an <code>int</code> value
     * @param c a <code>Choose</code> value
     */
    void setChoose(int i, Choose c);

    /** Adds a copy element to the list.
     * @param   c   copy element.
     */
    void addCopy(Copy c);
    
    /**
     * Describe <code>addForEach</code> method here.
     *
     * @param f a <code>ForEach</code> value
     */
    void addForEach(ForEach f);

    /**
     * Describe <code>addChoose</code> method here.
     *
     * @param c a <code>Choose</code> value
     */
    void addChoose(Choose c);

    /** Inserts copy element at given abstract index within list; 
     *  pushes all elements after to the right. Note that, the index passed in is 
     *  not the index within the copy list but the index within the children list
     * @param   i   Abstract Index of the element in the parent XMLElement
     * @param   c   copy element to insert.
     */
    void addCopy(int i, Copy c);
    
    /** Inserts ForEach element at given abstract index within list; 
     *  pushes all elements after to the right. Note that, the index passed in is 
     *  not the index within the ForEach list but the index within the children list
     * @param   i   Abstract Index of the element in the parent XMLElement
     * @param f a <code>ForEach</code> value
     */
    void addForEach(int i, ForEach f);

    /** Inserts Choose element at given abstract index within list; 
     *  pushes all elements after to the right. Note that, the index passed in is 
     *  not the index within the Choose list but the index within the children list
     * @param   i   Abstract Index of the element in the parent XMLElement
     * @param c a <code>Choose</code> value
     */
    void addChoose(int i, Choose c);

    /** Removes all the copy elements within the list.
     */
    void clearCopies();
    
    /**
     * Describe <code>clearForEachs</code> method here.
     *
     */
    void clearForEachs();

    /**
     * Describe <code>clearChooses</code> method here.
     *
     */
    void clearChooses();

    /** Removes a copy element from the list.
     * @param   i   Index to copy element.
     */
    void removeCopy(int i);
    
    /**
     * Describe <code>removeForEach</code> method here.
     *
     * @param i an <code>int</code> value
     */
    void removeForEach(int i);

    /**
     * Describe <code>removeChoose</code> method here.
     *
     * @param i an <code>int</code> value
     */
    void removeChoose(int i);

    /** Removes a copy element from the list.
     * @param   c   copy element.
     * @return  <tt>true</tt> if successfully removed.
     */
    boolean removeCopy(Copy c);
    
    /**
     * Describe <code>removeForEach</code> method here.
     *
     * @param f a <code>ForEach</code> value
     * @return a <code>boolean</code> value
     */
    boolean removeForEach(ForEach f);

    /**
     * Describe <code>removeChoose</code> method here.
     *
     * @param c a <code>Choose</code> value
     * @return a <code>boolean</code> value
     */
    boolean removeChoose(Choose c);

    /** Counts number of copy elements in list.
     * @return  list size.
     */
    int getCopySize();
    
    /**
     * Describe <code>getForEachSize</code> method here.
     *
     * @return an <code>int</code> value
     */
    int getForEachSize();

    /**
     * Describe <code>getChooseSize</code> method here.
     *
     * @return an <code>int</code> value
     */
    int getChooseSize();

    /** Gets index of copy element within list.
     * @param   copy    copy element to locate.
     * @return  Index of copy element within list.
     */
    int indexOfCopy(XMLNode copy);
    
    /**
     * Describe <code>indexOfForEach</code> method here.
     *
     * @param forEach a <code>XMLNode</code> value
     * @return an <code>int</code> value
     */
    int indexOfForEach(XMLNode forEach);

    /**
     * Describe <code>indexOfChoose</code> method here.
     *
     * @param choose a <code>XMLNode</code> value
     * @return an <code>int</code> value
     */
    int indexOfChoose(XMLNode choose);


    /** Gets collection of copy elements.
     * @return Unmodifiable collection of copy elements.
     */
    Collection getCopies();

    /**
     * Describe <code>getForEachs</code> method here.
     *
     * @return a <code>Collection</code> value
     */
    Collection getForEachs();

    /**
     * Describe <code>getChooses</code> method here.
     *
     * @return a <code>Collection</code> value
     */
    Collection getChooses();
  
    /** sets ExtensionAssignOperation 
     * @param   e   ExtensionAssignOperation element to insert.
     */
    public void setExtensionAssignOperation(ExtensionAssignOperation e);

    /** getter for ExtensionAssignOperation element.
     * @return  ExtensionAssignOperation element.
     */
    public ExtensionAssignOperation getExtensionAssignOperation();

    /** Getter for attribute validate.
     * @return Value of attribute validate.
     *
     */
    String getValidate();

    /** Setter for attribute validate.
     * @param validate - New value of attribute validate.
     *
     */
    void setValidate(String validate);
}
