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
 * @(#)ServiceAssemblyInfo.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.model.common;

import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;


/**
 * @author ylee
 * @author Graj
 *
 */

/**
 * ServiceAssemblyInfo
 */
public class ServiceAssemblyInfo {

    /** status  Unknown.  */
    public static final String UNKNOWN_STATUS = "UNKNOWN";

    /** status Deployed. */
    public static final String DEPLOYED_STATUS = "DEPLOYED";

    /** status Partially Deployed */
    public static final String PARTIALLY_DEPLOYED_STATUS = "PARTIALLY_DEPLOYED";

    /**
     * Holds value of property name.
     */
    private String name;

    /**
     * Holds value of property name.
     */
    private List<ServiceUnitInfo> suInfoList;

    /**
     * Holds value of property description.
     */
    private String description;

    /**
     * Holds value of property state.
     */
    private String status;

    /** Creates a new instance of JBIComponentInfo */
    public ServiceAssemblyInfo() {
        this("", "", DEPLOYED_STATUS);
    }

    /**
     * Creates a new instance of JBIComponentInfo
     * @param status fix it
     * @param id id
     * @param name name
     * @param description description
     */
    public ServiceAssemblyInfo(String name, String description, String status) {
        this.name = name;
        this.description = description;
        this.status = status;
        this.suInfoList = new ArrayList<ServiceUnitInfo>();
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
     * Getter for property state.
     * @return Value of property state.
     */
    public String getStatus() {
        return this.status;
    }

    /**
     * Setter for property state.
     * @param status fix it
     */
    public void setStatus(String status) {
        this.status = status;
    }

    /**
     * Getter for property description.
     * @return Value of property description.
     */
    public List getServiceUnitInfoList() {
        return this.suInfoList;
    }

    public void addServiceUnitInfoList(List<ServiceUnitInfo> suList) {
        suInfoList.addAll(suList);
    }
    
    /** string value
     * @return string value
     */
    public String toString() {
        return "Name = " + this.getName() + "\nDescription = "
                + this.getDescription() + "\nStatus = " + this.getStatus()
                + "\nSub Assemblies = " + this.getServiceUnitInfoList().size();

    }

    /** xml text
     * @return xml text
     */
    public String toXmlString() {
        List suInfoList = getServiceUnitInfoList();
        String suInfoText = ServiceUnitInfo.writeAsXmlText(suInfoList);

        return "<service-assembly-info>" + "<name>" + this.getName()
                + "</name>" + "<description>" + this.getDescription()
                + "</description>" + "<status>" + this.getStatus()
                + "</status>" + suInfoText + "</service-assembly-info> ";
    }

    /**
     * return component info object
     * @return object
     * @param auInfoEl fix it
     */
    public static ServiceAssemblyInfo createServiceAssemblyInfo(Element saInfoEl) {

        ServiceAssemblyInfo info = new ServiceAssemblyInfo();

        String name = null;
        Element nameEl = DOMUtil.util.getElement(saInfoEl, "name");
        if (nameEl != null) {
            name = DOMUtil.util.getTextData(nameEl);
        }
        info.setName(name);

        String desc = null;
        Element descEl = DOMUtil.util.getElement(saInfoEl, "description");
        if (descEl != null) {
            desc = DOMUtil.util.getTextData(descEl);
        }
        info.setDescription(desc);

        String status = null;
        Element statusEl = DOMUtil.util.getElement(saInfoEl, "status");
        if (statusEl != null) {
            status = DOMUtil.util.getTextData(statusEl);
        }
        info.setStatus(status);

        // construct the asa info list
        Element suInfoListEl = DOMUtil.util.getElement(saInfoEl,
                "service-unit-info-list");

        List<ServiceUnitInfo> list = ServiceUnitInfo.createServiceUnitInfoList(suInfoListEl);

        info.addServiceUnitInfoList(list);

        return info;
    }

    /**
     * creates list of objects
     * @return list of objects
     * @param auInfoListEl fix it
     */
    public static List<ServiceAssemblyInfo> createServiceAssemblyInfoList(Element saInfoListEl) {
        List<ServiceAssemblyInfo> saInfoList = new ArrayList<ServiceAssemblyInfo>();

        // construct the compInfo list
        if (saInfoListEl == null) {
            // System.out.println("No Root Element <compInfoList>");
            return saInfoList; // return empty list
        }

        NodeList saInfoNodeList = DOMUtil.util.getChildElements(saInfoListEl,
                "service-assembly-info");
        int size = saInfoNodeList.getLength();
        // System.out.println("compInfo found in XML Document : " + size);
        for (int i = 0; i < size; ++i) {
            Element saInfoEl = (Element) saInfoNodeList.item(i);
            if (saInfoEl != null) {
                ServiceAssemblyInfo saInfo = createServiceAssemblyInfo(saInfoEl);
                saInfoList.add(saInfo);
            }
        }

        return saInfoList;
    }

    /** creates list of objects
     * @param xmlReader xml text reader
     * @return list of objects
     */
    public static List readFromXmlTextWithProlog(Reader xmlReader) {
        ArrayList saInfoList = new ArrayList();
        Document xmlDoc = null;

        try {
            xmlDoc = DOMUtil.util.buildDOMDocument(xmlReader);
        } catch (Exception ex) {
            ex.printStackTrace();
        }

        if (xmlDoc == null) {
            return saInfoList; // return empty list
        }

        // construct the compInfo list
        Element saInfoListEl = DOMUtil.util.getElement(xmlDoc,
                "service-assembly-info-list");
        if (saInfoListEl == null) {
            // System.out.println("No Root Element <compInfoList>");
            return saInfoList; // return empty list
        }

        return createServiceAssemblyInfoList(saInfoListEl);
    }

    /** creates list of objects
     * @param xmlText xml text
     * @return list of objects
     */
    public static List readFromXmlTextWithProlog(String xmlText) {
        return readFromXmlTextWithProlog(new StringReader(xmlText));
    }

    /**
     * write to xml text
     * @return xml text
     * @param auInfoList fix it
     */
    public static String writeAsXmlText(List saInfoList) {
        StringWriter stringWriter = new StringWriter();
        PrintWriter writer = new PrintWriter(stringWriter, true);
        // begine xml
        writer.println("<service-assembly-info-list>");
        int size = saInfoList.size();
        for (int i = 0; i < size; ++i) {
            ServiceAssemblyInfo info = (ServiceAssemblyInfo) saInfoList.get(i);
            writer.println(info.toXmlString());
        }
        // end xml
        writer.println("</service-assembly-info-list>");

        try {
            writer.close();
            stringWriter.close();
        } catch (Exception ex) {
            // ex.printStackTrace();
            // ignore as it will never happen for strings
        }
        return stringWriter.toString();
    }

    /**
     * write to xml text
     * @return xml text
     * @param auInfoList fix it
     */
    public static String writeAsXmlTextWithProlog(List saInfoList) {
        StringBuffer buffer = new StringBuffer();
        // begine xml
        buffer.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
        buffer.append("\n");
        buffer.append(writeAsXmlText(saInfoList));
        buffer.append("\n");
        // end xml
        return buffer.toString();
    }

    /**
     * fix it
     * @param auDD fix it
     * @return fix it
     */
    public static ServiceAssemblyInfo createFromServiceAssemblyDD(
            ServiceAssemblyDD saDD) {

        ServiceAssemblyInfo saInfo = new ServiceAssemblyInfo();

        saInfo.setName(saDD.getName());
        saInfo.setDescription(saDD.getDescription());

        // init su info.
        List suDDList = saDD.getServiceUnitDDList();
        List<ServiceUnitInfo> suInfoList = new ArrayList<ServiceUnitInfo>();
        for (Iterator itr = suDDList.iterator(); itr.hasNext();) {
            ServiceUnitInfo suInfo = ServiceUnitInfo
                    .createFromServiceUnitDD((ServiceUnitDD) itr.next());
            suInfoList.add(suInfo);
        }
        saInfo.addServiceUnitInfoList(suInfoList);

        return saInfo;
    }

    /**
     * fix it
     * @param jbiXmlReader fix it
     * @return fix it
     */
    public static ServiceAssemblyInfo createFromServiceAssemblyDD(
            Reader jbiXmlReader) {
        try {
            ServiceAssemblyDD dd = (ServiceAssemblyDD) ServiceAssemblyDD
                    .createJBIDescriptor(jbiXmlReader);
            if (dd != null) {
                return createFromServiceAssemblyDD(dd);
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
        return null;
    }

    /**
     * doc me
     * @return doc me
     */
    public String getSelected() {
        String result = "Not Selected";
        if (mSelected) {
            result = "Selected";
        }
        return result;
    }

    /**
     * doc me
     * @return doc me
     */
    public boolean isSelected() {
        return mSelected;
    }

    /**
     * doc me
     * @param isSelected doc me
     */
    public void setSelected(boolean isSelected) {
        mSelected = isSelected;
    }

    /**
     * doc me
     */
    private boolean mSelected = false;

}
