/*
 * NamedItemList.java
 *
 * Created on April 7, 2007, 6:59 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.swiftbc.extensions;

/**
 *
 * @author Sun Microsystems, Inc.
 */
public interface SwiftNamedItemList {
        public int getNumberOfItem();

    /**
     * Method getNamedItem.
     * @param item int
     * @return NamedItem
     */
    public SwiftNamedItem getNamedItem(int item);

    /**
     * Method getNamedItem.
     * @param itemName String
     * @return NamedItem
     */
    public SwiftNamedItem getNamedItem(String itemName);

    /**
     * Method insert.
     * @param item NamedItem
     */
    public void insert(SwiftNamedItem item);

    /**
     * Method destroy.
     * @param item int
     */
    public void destroy(int item);

    /**
     * Method destroy.
     * @param itemName String
     */
    public void destroy(String itemName);
}
