/*
 * MockNamedItemList.java
 *
 * Created on April 7, 2007, 10:57 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package com.sun.jbi.swiftbc.extensions.mock;

import com.sun.jbi.swiftbc.extensions.SwiftNamedItem;
import com.sun.jbi.swiftbc.extensions.SwiftNamedItemList;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Sun Microsystems, Inc.
 */
public class MockNamedItemList implements SwiftNamedItemList{
    private Map<String,SwiftNamedItem> map = new HashMap<String,SwiftNamedItem>();
    /** Creates a new instance of MockNamedItemList */
    public MockNamedItemList() {
    }

    public SwiftNamedItem getNamedItem(String itemName) {
        return map.get(itemName);
    }

    public void destroy(String itemName) {
        map.remove(itemName);
    }

    public void insert(SwiftNamedItem item) {
        map.put(item.getItemName(),item);
    }

    public SwiftNamedItem getNamedItem(int item) {
        return null;
    }

    public void destroy(int item) {
    }

    public int getNumberOfItem() {
        return map.size();
    }
    
}
