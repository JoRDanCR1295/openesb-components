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
 * @(#)AspectMap.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.xml.configuration.model.aspects;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.sun.jbi.cam.common.EqualsUtil;
import com.sun.jbi.cam.common.HashCodeUtil;

/**
 * @author graj
 * 
 */
public class AspectMap implements Serializable {
    List<ServiceEntry> serviceEntryList = new ArrayList<ServiceEntry>();

    /**
     * 
     */
    public AspectMap() {
        // TODO Auto-generated constructor stub
    }

    /**
     * @return the serviceEntryList
     */
    public List<ServiceEntry> getServiceEntryList() {
        return serviceEntryList;
    }

    /**
     * @param serviceEntryList
     *            the serviceEntryList to set
     */
    public void setServiceEntryList(List<ServiceEntry> serviceEntryList) {
        this.serviceEntryList = serviceEntryList;
    }

    public boolean addServiceEntry(ServiceEntry entry) {
        return this.serviceEntryList.add(entry);
    }

    public boolean removeServiceEntry(ServiceEntry entry) {
        return this.serviceEntryList.remove(entry);
    }

    /**
     * Indicates whether some other object is "equal to" this one. The equals
     * method implements an equivalence relation on non-null object references:
     *  - It is reflexive: for any non-null reference value x, x.equals(x)
     * should return true. - It is symmetric: for any non-null reference values
     * x and y, x.equals(y) should return true if and only if y.equals(x)
     * returns true. - It is transitive: for any non-null reference values x, y,
     * and z, if x.equals(y) returns true and y.equals(z) returns true, then
     * x.equals(z) should return true. - It is consistent: for any non-null
     * reference values x and y, multiple invocations of x.equals(y)
     * consistently return true or consistently return false, provided no
     * information used in equals comparisons on the objects is modified. - For
     * any non-null reference value x, x.equals(null) should return false.
     * 
     * The equals method for class Object implements the most discriminating
     * possible equivalence relation on objects; that is, for any non-null
     * reference values x and y, this method returns true if and only if x and y
     * refer to the same object (x == y has the value true). Note that it is
     * generally necessary to override the hashCode method whenever this method
     * is overridden, so as to maintain the general contract for the hashCode
     * method, which states that equal objects must have equal hash codes.
     * 
     * @param aThat -
     *            the reference object with which to compare.
     * @returns true if this object is the same as the obj argument; false
     *          otherwise.
     */
    public boolean equals(Object aThat) {
        // check for self-comparison
        if (this == aThat) {
            return true;
        }

        // use instanceof instead of getClass here for two reasons
        // 1. if need be, it can match any supertype, and not just one class;
        // 2. it renders an explict check for "that == null" redundant, since
        // it does the check for null already - "null instanceof [type]" always
        // returns false. (See Effective Java by Joshua Bloch.)
        if (!(aThat instanceof AspectMap)) {
            return false;
        }
        // Alternative to the above line :
        // if ( aThat == null || aThat.getClass() != this.getClass() ) return
        // false;

        // cast to native object is now safe
        AspectMap that = (AspectMap) aThat;

        // now a proper field-by-field evaluation can be made
        return EqualsUtil
                .areEqual(this.serviceEntryList, that.serviceEntryList);
    }

    /**
     * Returns a hash code value for the object. This method is supported for
     * the benefit of hashtables such as those provided by java.util.Hashtable.
     * The general contract of hashCode is: - Whenever it is invoked on the same
     * object more than once during an execution of a Java application, the
     * hashCode method must consistently return the same integer, provided no
     * information used in equals comparisons on the object is modified. This
     * integer need not remain consistent from one execution of an application
     * to another execution of the same application. - If two objects are equal
     * according to the equals(Object) method, then calling the hashCode method
     * on each of the two objects must produce the same integer result. - It is
     * not required that if two objects are unequal according to the
     * equals(java.lang.Object) method, then calling the hashCode method on each
     * of the two objects must produce distinct integer results. However, the
     * programmer should be aware that producing distinct integer results for
     * unequal objects may improve the performance of hashtables. As much as is
     * reasonably practical, the hashCode method defined by class Object does
     * return distinct integers for distinct objects. (This is typically
     * implemented by converting the internal address of the object into an
     * integer, but this implementation technique is not required by the JavaTM
     * programming language.)
     * 
     * @returns a hash code value for this object.
     */
    public int hashCode() {
        // this style of lazy initialization is
        // suitable only if the object is immutable
        int fHashCode = 0;
        if (fHashCode == 0) {
            int result = HashCodeUtil.SEED;
            result = HashCodeUtil.hash(result, this.serviceEntryList);
            fHashCode = result;
        }
        return fHashCode;
    }

    public void dump() {
        System.out.println("Aspect Map is:");
        Iterator<ServiceEntry> iterator = this.serviceEntryList.iterator();
        while (iterator.hasNext() == true) {
            ServiceEntry entry = iterator.next();
            entry.dump();
            System.out.println("");
        }

    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        // TODO Auto-generated method stub

    }

}
