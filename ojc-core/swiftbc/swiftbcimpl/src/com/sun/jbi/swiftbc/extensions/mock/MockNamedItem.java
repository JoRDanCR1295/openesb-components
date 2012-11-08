/*
 * MockNamedItem.java
 *
 * Created on April 7, 2007, 8:08 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.swiftbc.extensions.mock;
import com.sun.jbi.swiftbc.extensions.SwiftNamedItem;
import com.sun.jbi.swiftbc.extensions.SwiftNamedItemList;
/**
 *
 * @author Sun Microsystems, Inc.
 */
public class MockNamedItem implements SwiftNamedItem{
    private String itemValue;
    private String itemName;
    private SwiftNamedItemList nitemList;
    
    /** Creates a new instance of MockNamedItem */
    public MockNamedItem() {
    }

    public void setItemValue(String value) {
        itemValue = value;
    }

    public void setItemName(String name) {
        itemName = name;
    }

    public void setNamedItemList(SwiftNamedItemList itemList) {
        nitemList = itemList;
    }

    public boolean isItemList() {
        return true;
    }

    public SwiftNamedItemList getNamedItemList() {
        return nitemList;
    }

    public String getItemValue() {
        return itemValue;
    }

    public String getItemName() {
        return itemName;
    }
    
}
