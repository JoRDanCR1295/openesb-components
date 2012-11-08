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
 * @(#)JBIComponentStatusDocument.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.model.management;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.sun.jbi.cam.model.management.JBIComponentStatus;

/**
 * @author ylee
 * @author Graj
 *
 */
public class JBIComponentStatusDocument implements Serializable {

    public static final String COMP_INFO_LIST_NODE_NAME = "component-info-list";
    public static final String COMP_INFO_NODE_NAME = "component-info";
//    public static final String ID_NODE_NAME = "id";
    public static final String NAME_NODE_NAME = "name";
    public static final String TYPE_NODE_NAME = "type";
    public static final String STATUS_NODE_NAME = "state";
    public static final String DESCRIPTION_NODE_NAME = "description";
    public static final String VERSION_NODE_NAME = "version";
    public static final String NAMESPACE_NODE_NAME = "xmlns";

    protected List<JBIComponentStatus> jbiComponentStatusList = new ArrayList<JBIComponentStatus>();


    /**
     *
     */
    public JBIComponentStatusDocument() {
    }


    /**
     * @return Returns the jbiComponentList.
     */
    public List<JBIComponentStatus> getJbiComponentStatusList() {
        return this.jbiComponentStatusList;
    }
    
    /**
     * @param jbiComponentList The jbiComponentList to set.
     */
    public void setJbiComponentStatusList(List<JBIComponentStatus> jbiComponentStatusList) {
        this.jbiComponentStatusList = jbiComponentStatusList;
    }

    public void addJbiComponentStatus(JBIComponentStatus componentStatus) {
        jbiComponentStatusList.add(componentStatus);
    } 
    
    public void addJbiComponentStatusList(List<JBIComponentStatus> list) {
        jbiComponentStatusList.addAll(list);
    }
    
    public void dump() {
        Iterator iterator = this.jbiComponentStatusList.iterator();
        JBIComponentStatus component = null;
        while((iterator != null) && (iterator.hasNext() == true)) {
            component = (JBIComponentStatus) iterator.next();
            component.dump();
        }

    }
    public static void main(String[] args) {
    }
}
