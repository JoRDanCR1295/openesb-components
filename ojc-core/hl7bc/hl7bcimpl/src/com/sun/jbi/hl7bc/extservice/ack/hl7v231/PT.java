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
 * @(#)PT.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/


package com.sun.jbi.hl7bc.extservice.ack.hl7v231;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.PT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.PT1CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.PT2CONTENT;


/**
 * <p>Java class for PT complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="PT">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}PT.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PT.2" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "PT", namespace = "urn:hl7-org:v2xml", propOrder = {
    "pt1",
    "pt2"
})
public class PT {

    @XmlElement(name = "PT.1", namespace = "urn:hl7-org:v2xml")
    protected PT1CONTENT pt1;
    @XmlElement(name = "PT.2", namespace = "urn:hl7-org:v2xml")
    protected PT2CONTENT pt2;

    /**
     * Gets the value of the pt1 property.
     * 
     * @return
     *     possible object is
     *     {@link PT1CONTENT }
     *     
     */
    public PT1CONTENT getPT1() {
        return pt1;
    }

    /**
     * Sets the value of the pt1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PT1CONTENT }
     *     
     */
    public void setPT1(PT1CONTENT value) {
        this.pt1 = value;
    }

    /**
     * Gets the value of the pt2 property.
     * 
     * @return
     *     possible object is
     *     {@link PT2CONTENT }
     *     
     */
    public PT2CONTENT getPT2() {
        return pt2;
    }

    /**
     * Sets the value of the pt2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PT2CONTENT }
     *     
     */
    public void setPT2(PT2CONTENT value) {
        this.pt2 = value;
    }

}
