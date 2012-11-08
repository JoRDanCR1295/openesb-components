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
 * @(#)MSACONTENT.java 
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
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.MSA1CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.MSA2CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.MSA3CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.MSA4CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.MSA5CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.MSA6CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.MSACONTENT;
import org.w3c.dom.Element;


/**
 * <p>Java class for MSA.CONTENT complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="MSA.CONTENT">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}MSA.1"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}MSA.2"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}MSA.3" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}MSA.4" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}MSA.5" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}MSA.6" minOccurs="0"/>
 *         &lt;any/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "MSA.CONTENT", namespace = "urn:hl7-org:v2xml", propOrder = {
    "msa1",
    "msa2",
    "msa3",
    "msa4",
    "msa5",
    "msa6",
    "any"
})
public class MSACONTENT {

    @XmlElement(name = "MSA.1", namespace = "urn:hl7-org:v2xml")
    protected MSA1CONTENT msa1;
    @XmlElement(name = "MSA.2", namespace = "urn:hl7-org:v2xml")
    protected MSA2CONTENT msa2;
    @XmlElement(name = "MSA.3", namespace = "urn:hl7-org:v2xml")
    protected MSA3CONTENT msa3;
    @XmlElement(name = "MSA.4", namespace = "urn:hl7-org:v2xml")
    protected MSA4CONTENT msa4;
    @XmlElement(name = "MSA.5", namespace = "urn:hl7-org:v2xml")
    protected MSA5CONTENT msa5;
    @XmlElement(name = "MSA.6", namespace = "urn:hl7-org:v2xml")
    protected MSA6CONTENT msa6;
    @XmlAnyElement(lax = true)
    protected Object any;

    /**
     * Gets the value of the msa1 property.
     * 
     * @return
     *     possible object is
     *     {@link MSA1CONTENT }
     *     
     */
    public MSA1CONTENT getMSA1() {
        return msa1;
    }

    /**
     * Sets the value of the msa1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link MSA1CONTENT }
     *     
     */
    public void setMSA1(MSA1CONTENT value) {
        this.msa1 = value;
    }

    /**
     * Gets the value of the msa2 property.
     * 
     * @return
     *     possible object is
     *     {@link MSA2CONTENT }
     *     
     */
    public MSA2CONTENT getMSA2() {
        return msa2;
    }

    /**
     * Sets the value of the msa2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link MSA2CONTENT }
     *     
     */
    public void setMSA2(MSA2CONTENT value) {
        this.msa2 = value;
    }

    /**
     * Gets the value of the msa3 property.
     * 
     * @return
     *     possible object is
     *     {@link MSA3CONTENT }
     *     
     */
    public MSA3CONTENT getMSA3() {
        return msa3;
    }

    /**
     * Sets the value of the msa3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link MSA3CONTENT }
     *     
     */
    public void setMSA3(MSA3CONTENT value) {
        this.msa3 = value;
    }

    /**
     * Gets the value of the msa4 property.
     * 
     * @return
     *     possible object is
     *     {@link MSA4CONTENT }
     *     
     */
    public MSA4CONTENT getMSA4() {
        return msa4;
    }

    /**
     * Sets the value of the msa4 property.
     * 
     * @param value
     *     allowed object is
     *     {@link MSA4CONTENT }
     *     
     */
    public void setMSA4(MSA4CONTENT value) {
        this.msa4 = value;
    }

    /**
     * Gets the value of the msa5 property.
     * 
     * @return
     *     possible object is
     *     {@link MSA5CONTENT }
     *     
     */
    public MSA5CONTENT getMSA5() {
        return msa5;
    }

    /**
     * Sets the value of the msa5 property.
     * 
     * @param value
     *     allowed object is
     *     {@link MSA5CONTENT }
     *     
     */
    public void setMSA5(MSA5CONTENT value) {
        this.msa5 = value;
    }

    /**
     * Gets the value of the msa6 property.
     * 
     * @return
     *     possible object is
     *     {@link MSA6CONTENT }
     *     
     */
    public MSA6CONTENT getMSA6() {
        return msa6;
    }

    /**
     * Sets the value of the msa6 property.
     * 
     * @param value
     *     allowed object is
     *     {@link MSA6CONTENT }
     *     
     */
    public void setMSA6(MSA6CONTENT value) {
        this.msa6 = value;
    }

    /**
     * Gets the value of the any property.
     * 
     * @return
     *     possible object is
     *     {@link Element }
     *     {@link Object }
     *     
     */
    public Object getAny() {
        return any;
    }

    /**
     * Sets the value of the any property.
     * 
     * @param value
     *     allowed object is
     *     {@link Element }
     *     {@link Object }
     *     
     */
    public void setAny(Object value) {
        this.any = value;
    }

}
