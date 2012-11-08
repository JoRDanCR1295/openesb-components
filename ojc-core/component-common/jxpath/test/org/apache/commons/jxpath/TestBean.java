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
 * @(#)TestBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.apache.commons.jxpath;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.jxpath.util.ValueUtils;

/**
 * General purpose test bean for JUnit tests for the "jxpath" component.
 *
 * @author Dmitri Plotnikov
 * @version  
 */
public class TestBean {

    // ------------------------------------------------------------- Properties

    /**
     * An array of nested java beans.
     */
    private NestedTestBean[] beans;
    {
        beans = new NestedTestBean[2];
        beans[0] = new NestedTestBean("Name 1");
        beans[1] = new NestedTestBean("Name 2");
        beans[1].setInt(3);
    }

    public NestedTestBean[] getBeans() {
        return beans;
    }

    public void setBeans(NestedTestBean[] beans) {
        this.beans = beans;
    }

    /**
     * A boolean property.
     */
    private boolean bool = false;
    public boolean getBoolean() {
        return bool;
    }

    public void setBoolean(boolean bool) {
        this.bool = bool;
    }

    private int integer = 1;
    /**
     * A read-only integer property
     */
    public int getInt() {
        return integer;
    }

    public void setInt(int integer) {
        this.integer = integer;
    }

    /**
     * A read-only array of integers
     */
    private int[] array = { 1, 2, 3, 4 };
    public int[] getIntegers() {
        return array;
    }

    public int getIntegers(int index) {
        return array[index];
    }

    public void setIntegers(int index, int value) {
        if (index >= array.length) {
            array = (int[]) ValueUtils.expandCollection(array, index + 1);
        }
        array[index] = value;
    }

    /**
     * A heterogeneous list: String, Integer, NestedTestBean
     */
    private ArrayList list;
    public List getList() {
        if (list == null) {
            list = new ArrayList();
            list.add("String 3");
            list.add(new Integer(3));
            list.add(new NestedTestBean("Name 3"));
        }
        return list;
    }

    /**
     * A Map
     */
    private HashMap map;
    {
        map = new HashMap();
        map.put("Key1", "Value 1");
        map.put("Key2", new NestedTestBean("Name 6"));
    }

    public Map getMap() {
        return map;
    }

    public void setMap(Map map) {
        this.map = (HashMap) map;
    }

    /**
     * A nested read-only java bean
     */
    private NestedTestBean nestedBean = new NestedTestBean("Name 0");
    public NestedTestBean getNestedBean() {
        return nestedBean;
    }

    public void setNestedBean(NestedTestBean bean) {
        this.nestedBean = bean;
    }

    private NestedTestBean object = new NestedTestBean("Name 5");

    /**
     * Returns a NestedTestBean: testing recognition of generic objects
     */
    public Object getObject() {
        return object;
    }

    /**
     * Returns an array of ints: testing recognition of generic objects
     */
    public Object getObjects() {
        return getIntegers();
    }

    /**
     * A heterogeneous set: String, Integer, NestedTestBean
     */
    private HashSet set;
    public Set getSet() {
        if (set == null) {
            set = new HashSet();
            set.add("String 4");
            set.add(new Integer(4));
            set.add(new NestedTestBean("Name 4"));
        }
        return set;
    }

    public String toString() {
        return "ROOT";
    }
}
