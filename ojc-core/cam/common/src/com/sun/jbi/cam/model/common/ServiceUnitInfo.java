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
 * @(#)ServiceUnitInfo.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.model.common;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

/**
 * @author ylee
 * @author Graj
 *
 */

import com.sun.jbi.cam.model.common.ServiceUnitDD;

/**
 * ServiceUnitInfo
 */
public class ServiceUnitInfo {

    /** status  Unknown.  */
    public static final String UNKNOWN_STATUS = "UNKNOWN";

    /** status Deployed. */
    public static final String DEPLOYED_STATUS = "DEPLOYED";

    /** status Deployed. */
    public static final String START_STATUS = "START";

    /** status Deployed. */
    public static final String STOP_STATUS = "STOP";

    /** status Deployed. */
    public static final String SHUTDOWN_STATUS = "SHUTDOWN";

    /**
     * Holds value of property name.
     */
    private String name;

    /**
     * Holds value of property description.
     */
    private String description;

    /**
     * Holds value of property targetId.
     */
    private String targetName;

    /**
     * Holds value of property state.
     */
    private String status;

    /** Creates a new instance of JBIComponentInfo */
    public ServiceUnitInfo() {
        this("", "", "", UNKNOWN_STATUS);
    }

    /**
     * Creates a new instance of SunAssemblyInfo
     * @param targetId target id
     * @param status status
     * @param id id
     * @param name name
     * @param description description
     */
    public ServiceUnitInfo(String name, String description, String targetName,
            String status) {
        this.name = name;
        this.description = description;
        this.targetName = targetName;
        this.status = status;
    }

    /**
     * Getter for property aliasName.
     * @return Value of property aliasName.
     */
    public String getName() {
        return this.name;
    }

    /**
     * Setter for property aliasName.
     * @param name New value of property aliasName.
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Getter for property description.
     * @return Value of property description.
     */
    public String getDescription() {
        return this.description;
    }

    /**
     * Setter for property description.
     * @param description New value of property description.
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * Getter for property targetId.
     * @return Value of property targetId.
     */
    public String getTargetName() {
        return this.targetName;
    }

    /**
     * Setter for property targetId.
     * @param targetName New value of property targetId.
     */
    public void setTargetName(String targetName) {
        this.targetName = targetName;
    }

    /**
     * Getter for property state.
     * @return Value of property state.
     */
    public String getStatus() {
        return this.status;
    }

    /**
     * Setter for property status.
     * @param status status
     */
    public void setStatus(String status) {
        this.status = status;
    }

    /** string value
     * @return string value
     */
    public String toString() {
        return "Name = " + this.getName() + "\nDescription = "
                + this.getDescription() + "\nTarget Name = "
                + this.getTargetName() + "\nStatus = " + this.getStatus();

    }

    /** xml text
     * @return xml text
     */
    public String toXmlString() {
        return "<service-unit-info>" + "<name>" + this.getName() + "</name>"
                + "<description>" + this.getDescription() + "</description>"
                + "<status>" + this.getStatus() + "</status>" + "<target-name>"
                + this.getTargetName() + "</target-name>"
                + "</service-unit-info> ";
    }

    /**
     * return sub assembly info object
     * @return object
     * @param subAssemblyInfoEl element
     */
    public static ServiceUnitInfo createServiceUnitInfo(Element suInfoEl) {

        ServiceUnitInfo info = new ServiceUnitInfo();

        if (suInfoEl == null) {
            return null;
        }
        // TODO. check the element has the tag name "service-unit-info" or not

        String name = null;
        Element nameEl = DOMUtil.util.getElement(suInfoEl, "name");
        if (nameEl != null) {
            name = DOMUtil.util.getTextData(nameEl);
        }
        info.setName(name);

        String desc = null;
        Element descEl = DOMUtil.util.getElement(suInfoEl, "description");
        if (descEl != null) {
            desc = DOMUtil.util.getTextData(descEl);
        }
        info.setDescription(desc);

        String status = null;
        Element statusEl = DOMUtil.util.getElement(suInfoEl, "status");
        if (statusEl != null) {
            status = DOMUtil.util.getTextData(statusEl);
        }
        info.setStatus(status);

        String targetName = null;
        Element targetNameEl = DOMUtil.util.getElement(suInfoEl, "target-name");
        if (targetNameEl != null) {
            targetName = DOMUtil.util.getTextData(targetNameEl);
        }
        info.setTargetName(targetName);

        return info;
    }

    /**
     * return component info object
     * @return object
     * @param asaInfoListEl element
     */
    public static List<ServiceUnitInfo> createServiceUnitInfoList(Element suInfoListEl) {
        List<ServiceUnitInfo> suInfoList = new ArrayList<ServiceUnitInfo>();

        if (suInfoListEl == null) {
            // System.out.println("No Root Element <service-unit-info-list>");
            return suInfoList; // return empty list
        }
        // TODO: check if suInfoListElement is service-unit-info-list or not.

        NodeList suInfoNodeList = DOMUtil.util.getChildElements(suInfoListEl,
                "service-unit-info");

        if (suInfoNodeList == null) {
            return suInfoList;
        }

        int size = suInfoNodeList.getLength();
        // System.out.println("compInfo found in XML Document : " + size);
        for (int i = 0; i < size; ++i) {
            Element suInfoEl = (Element) suInfoNodeList.item(i);
            if (suInfoEl != null) {
                ServiceUnitInfo suInfo = createServiceUnitInfo(suInfoEl);
                suInfoList.add(suInfo);
            }
        }

        return suInfoList;

    }

    /** creates list of objects
     * @param xmlText xml text
     * @return list of objects
     */
    public static List readFromXmlTextWithProlog(String xmlText) {
        ArrayList suInfoList = new ArrayList();
        Document xmlDoc = null;

        try {
            xmlDoc = DOMUtil.util.buildDOMDocument(xmlText);
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        if (xmlDoc == null) {
            return suInfoList; // return empty list
        }

        // construct the asa info list
        Element suInfoListEl = DOMUtil.util.getElement(xmlDoc,
                "service-unit-info-list");

        return createServiceUnitInfoList(suInfoListEl);

    }

    /**
     * write to xml text
     * @return xml text
     * @param asaInfoList list
     */
    public static String writeAsXmlText(List suInfoList) {
        StringWriter stringWriter = new StringWriter();
        PrintWriter writer = new PrintWriter(stringWriter, true);
        // begine xml
        writer.println("<service-unit-info-list>");
        if (suInfoList != null) {
            int size = suInfoList.size();
            for (int i = 0; i < size; ++i) {
                ServiceUnitInfo info = (ServiceUnitInfo) suInfoList.get(i);
                writer.println(info.toXmlString());
            }
            // end xml
            writer.println("</service-unit-info-list>");
        }
        try {
            writer.close();
            stringWriter.close();
        } catch (Exception ex) {
            // ex.printStackTrace();
            // ignore as it will never happen for strings unless runtime exp as mem exp
        }
        return stringWriter.toString();
    }

    /**
     * write to xml text
     * @return xml text
     * @param asaInfoList list
     */
    public static String writeAsXmlTextWithProlog(List suInfoList) {
        StringBuffer buffer = new StringBuffer();
        // begin xml
        buffer.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
        buffer.append("\n");
        buffer.append(writeAsXmlText(suInfoList));
        buffer.append("\n");
        // end xml
        return buffer.toString();

    }

    /**
     * creates the info from jbi.xml
     * @param asaDD object constructed from jbi.xml
     * @return info object
     */
    public static ServiceUnitInfo createFromServiceUnitDD(ServiceUnitDD suDD) {

        ServiceUnitInfo suInfo = new ServiceUnitInfo();

        suInfo.setName(suDD.getName());
        suInfo.setDescription(suDD.getDescription());
        suInfo.setTargetName(suDD.getTargetName());

        return suInfo;
    }

}
