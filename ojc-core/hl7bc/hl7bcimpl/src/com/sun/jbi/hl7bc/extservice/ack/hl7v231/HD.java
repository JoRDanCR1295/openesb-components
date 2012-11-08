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
 * @(#)HD.java 
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
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.HD;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.HD1CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.HD2CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.HD3CONTENT;


/**
 * <p>Java class for HD complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="HD">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}HD.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}HD.2" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}HD.3" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "HD", namespace = "urn:hl7-org:v2xml", propOrder = {
    "hd1",
    "hd2",
    "hd3"
})
public class HD {

    @XmlElement(name = "HD.1", namespace = "urn:hl7-org:v2xml")
    protected HD1CONTENT hd1;
    @XmlElement(name = "HD.2", namespace = "urn:hl7-org:v2xml")
    protected HD2CONTENT hd2;
    @XmlElement(name = "HD.3", namespace = "urn:hl7-org:v2xml")
    protected HD3CONTENT hd3;

    /**
     * Gets the value of the hd1 property.
     * 
     * @return
     *     possible object is
     *     {@link HD1CONTENT }
     *     
     */
    public HD1CONTENT getHD1() {
        return hd1;
    }

    /**
     * Sets the value of the hd1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link HD1CONTENT }
     *     
     */
    public void setHD1(HD1CONTENT value) {
        this.hd1 = value;
    }

    /**
     * Gets the value of the hd2 property.
     * 
     * @return
     *     possible object is
     *     {@link HD2CONTENT }
     *     
     */
    public HD2CONTENT getHD2() {
        return hd2;
    }

    /**
     * Sets the value of the hd2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link HD2CONTENT }
     *     
     */
    public void setHD2(HD2CONTENT value) {
        this.hd2 = value;
    }

    /**
     * Gets the value of the hd3 property.
     * 
     * @return
     *     possible object is
     *     {@link HD3CONTENT }
     *     
     */
    public HD3CONTENT getHD3() {
        return hd3;
    }

    /**
     * Sets the value of the hd3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link HD3CONTENT }
     *     
     */
    public void setHD3(HD3CONTENT value) {
        this.hd3 = value;
    }

}
