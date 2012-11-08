/*
 * NamedItem.java
 *
 * Created on April 7, 2007, 6:56 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.swiftbc.extensions;

/**
 *
 * @author Sun Microsystems, Inc.
 */
public interface SwiftNamedItem {
    /** 
     * This member is used to store the address of its SAG C++ object.
     * Would like to declare it as private since it should never be
     * referenced on the Java side.  However, it is referenced
     * by other classes on the JNI C++ side and is therefore declared
     * as public. (Harry - It doesn't have to be public, declare it as private).
     */


    /**
     * Method setItemName.
     * @param name String
     */
    public void setItemName(String name);

    /**
     * Method setItemValue.
     * @param value String
     */
    public void setItemValue(String value);

    /**
     * Method setNamedItemList.
     * @param itemList NamedItemList
     */
    public void setNamedItemList(SwiftNamedItemList itemList);

    /**
     * Method getItemName.
     * @return String
     */
    public String getItemName();

    /**
     * Method getItemValue.
     * @return String
     */
    public String getItemValue();

    /**
     * Method getNamedItemList.
     * @return NamedItemList
     */
    public SwiftNamedItemList getNamedItemList();

    /**
     * Method isItemList.
     * @return boolean
     */
    public boolean isItemList();

}

