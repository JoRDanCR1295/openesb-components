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
 * @(#)ServiceAssemblyDD.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.model.common;

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * @author ylee
 * @author Graj
 *
 */

/**
 * This class is the java model for ServiceAssembly representation of jbi.xml.
 * Right now it is constructed from the DOM model. In future it will be
 * constructed from the JAXB model.
 */
public class ServiceAssemblyDD extends JBIDescriptor {
    /**
     */
    Identification idInfo;

    /**
     */
    private List<ServiceUnitDD> serviceUnitDDList;

    /**
     * Creates a new instance of JBIDescriptor
     * @param xmlText text
     * @throws Exception on error
     */
    protected ServiceAssemblyDD(Identification idInfo, List<ServiceUnitDD> suDDList) {
        this.idInfo = idInfo;
        if (suDDList != null) {
            this.serviceUnitDDList = Collections.unmodifiableList(suDDList);
        } else {
            this.serviceUnitDDList = new ArrayList<ServiceUnitDD>();
        }
    }

    /**
     * attribute
     * @return value
     */
    public String getName() {
        return this.idInfo.getName();
    }

    /**
     * attribute
     * @return value
     */
    public String getDescription() {
        return this.idInfo.getDescription();
    }

    /**
     * fix it
     * @return fix it
     */
    public List getServiceUnitDDList() {
        return this.serviceUnitDDList;
    }

    public boolean isSharedLibraryDescriptor() {
        return false;
    }

    public boolean isServiceEngineDescriptor() {
        return false;
    }

    public boolean isBindingComponentDescriptor() {
        return false;
    }

    public boolean isServiceAssemblyDescriptor() {
        return true;
    }

    /**
     * object as string
     * @return string
     */
    public String toString() {
        //        if ( this.mIdInfo == null )
        //        {
        //            return "Identification object is null in ServiceAssemblyDD";
        //        }
        //        if( this.mServiceUnitDDList == null )
        //        {
        //            return "ServiceUnitDDList is null in ServiceAssemblyDD";
        //        }

        return "Name : " + getName() + "\n" + "Description : "
                + getDescription() + "\n" + "Service Units : "
                + getServiceUnitDDList().size();
    }

    /**
     * object as string
     * @return string
     */
    public void printServiceAssembly(PrintStream out) {
        out.println(this);
        List list = this.getServiceUnitDDList();
        for (Iterator itr = list.iterator(); itr.hasNext();) {
            out.println(itr.next());
        }
    }

    public static ServiceAssemblyDD createServiceAssemblyDD(
            Identification idInfo, List<ServiceUnitDD> suDDList) {
        return new ServiceAssemblyDD(idInfo, suDDList);
    }

    public static ServiceAssemblyDD createServiceAssemblyDD(Element saEl) {
        Identification idInfo = null;
        List<ServiceUnitDD> suDDList = new ArrayList<ServiceUnitDD>();

        Element idInfoEl = DOMUtil.util.getChildElement(saEl, "identification");
        if (idInfoEl != null) {
            idInfo = Identification.createIdentification(idInfoEl);
        }
        NodeList suNodeList = DOMUtil.util.getChildElements(saEl,
                "service-unit");
        int size = suNodeList.getLength();
        for (int i = 0; i < size; ++i) {
            Element suEl = (Element) suNodeList.item(i);
            if (suEl != null) {
                ServiceUnitDD suDD = ServiceUnitDD.createServiceUnitDD(suEl);
                suDDList.add(suDD);
            }
        }
        return createServiceAssemblyDD(idInfo, suDDList);
    }
}
