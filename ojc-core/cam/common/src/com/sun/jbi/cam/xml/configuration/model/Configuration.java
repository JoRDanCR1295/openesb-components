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
 * @(#)Configuration.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 * 
 */
package com.sun.jbi.cam.xml.configuration.model;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

/**
 * @author graj
 * 
 */
public class Configuration implements Serializable {
    String name;

    Map<String, DisplayInformation> displayDetailsMap = new HashMap<String, DisplayInformation>();

    Map<String, DisplayInformation> labelDisplayDetailsMap = new HashMap<String, DisplayInformation>();

    /**
     * 
     */
    public Configuration() {
        // TODO Auto-generated constructor stub
    }

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String componentName) {
        this.name = componentName;
    }

    /**
     * @return the displayDetailsMap
     */
    public Map<String, DisplayInformation> getDisplayDetailsMap() {
        return displayDetailsMap;
    }

    public Map<String, DisplayInformation> getLabelDisplayDetailsMap() {
        return labelDisplayDetailsMap;
    }

    /**
     * @param displayDetailsMap
     *            the displayDetailsMap to set
     */
    public void setDisplayDetailsMap(
            Map<String, DisplayInformation> displayDetailsMap) {
        this.displayDetailsMap = displayDetailsMap;
        Set<Entry<String, DisplayInformation>> set = displayDetailsMap
                .entrySet();
        Entry<String, DisplayInformation> entry = null;
        for (Iterator<Entry<String, DisplayInformation>> iterator = set
                .iterator(); iterator.hasNext() == true;) {
            entry = iterator.next();
            if (entry != null) {
                String ket = entry.getKey();
                DisplayInformation info = entry.getValue();
                if (info != null) {
                    this.labelDisplayDetailsMap
                            .put(info.getDisplayName(), info);
                }
            }
        }
    }

    /**
     * @param displayDetailsMap
     *            the displayDetailsMap to set
     */
    public void setLabelDisplayDetailsMap(
            Map<String, DisplayInformation> labelDisplayDetailsMap) {
        this.labelDisplayDetailsMap = labelDisplayDetailsMap;
        Set<Entry<String, DisplayInformation>> set = labelDisplayDetailsMap
                .entrySet();
        Entry<String, DisplayInformation> entry = null;
        for (Iterator<Entry<String, DisplayInformation>> iterator = set
                .iterator(); iterator.hasNext() == true;) {
            entry = iterator.next();
            if (entry != null) {
                String ket = entry.getKey();
                DisplayInformation info = entry.getValue();
                if (info != null) {
                    this.displayDetailsMap.put(info.getAttributeName(), info);
                }
            }
        }
    }

    /**
     * 
     * @param attributeName
     * @param value
     * @return
     */
    public DisplayInformation addDisplayDetail(String attributeName,
            DisplayInformation value) {
        DisplayInformation returnValue = null;
        if ((attributeName != null) && (value != null)) {
            returnValue = this.displayDetailsMap.put(attributeName, value);
            this.labelDisplayDetailsMap.put(value.getDisplayName(), value);
        }
        return returnValue;
    }

    public void dump() {
        System.out.println("name: " + name);
        Entry<String, DisplayInformation> entry = null;
        String key = null;
        DisplayInformation value = null;
        Set<Entry<String, DisplayInformation>> set = this.displayDetailsMap
                .entrySet();
        Iterator<Entry<String, DisplayInformation>> iterator = set.iterator();
        while (iterator.hasNext() == true) {
            entry = iterator.next();
            if (entry != null) {
                key = entry.getKey();
                value = entry.getValue();
                value.dump();
            }
        }
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        // TODO Auto-generated method stub

    }

}
