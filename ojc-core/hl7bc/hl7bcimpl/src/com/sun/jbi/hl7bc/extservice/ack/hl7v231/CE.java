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
 * @(#)CE.java 
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
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.CE;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.CE1CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.CE2CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.CE3CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.CE4CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.CE5CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v231.CE6CONTENT;


/**
 * <p>Java class for CE complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="CE">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}CE.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CE.2" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CE.3" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CE.4" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CE.5" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}CE.6" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "CE", namespace = "urn:hl7-org:v2xml", propOrder = {
    "ce1",
    "ce2",
    "ce3",
    "ce4",
    "ce5",
    "ce6"
})
public class CE {

    @XmlElement(name = "CE.1", namespace = "urn:hl7-org:v2xml")
    protected CE1CONTENT ce1;
    @XmlElement(name = "CE.2", namespace = "urn:hl7-org:v2xml")
    protected CE2CONTENT ce2;
    @XmlElement(name = "CE.3", namespace = "urn:hl7-org:v2xml")
    protected CE3CONTENT ce3;
    @XmlElement(name = "CE.4", namespace = "urn:hl7-org:v2xml")
    protected CE4CONTENT ce4;
    @XmlElement(name = "CE.5", namespace = "urn:hl7-org:v2xml")
    protected CE5CONTENT ce5;
    @XmlElement(name = "CE.6", namespace = "urn:hl7-org:v2xml")
    protected CE6CONTENT ce6;

    /**
     * Gets the value of the ce1 property.
     * 
     * @return
     *     possible object is
     *     {@link CE1CONTENT }
     *     
     */
    public CE1CONTENT getCE1() {
        return ce1;
    }

    /**
     * Sets the value of the ce1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CE1CONTENT }
     *     
     */
    public void setCE1(CE1CONTENT value) {
        this.ce1 = value;
    }

    /**
     * Gets the value of the ce2 property.
     * 
     * @return
     *     possible object is
     *     {@link CE2CONTENT }
     *     
     */
    public CE2CONTENT getCE2() {
        return ce2;
    }

    /**
     * Sets the value of the ce2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CE2CONTENT }
     *     
     */
    public void setCE2(CE2CONTENT value) {
        this.ce2 = value;
    }

    /**
     * Gets the value of the ce3 property.
     * 
     * @return
     *     possible object is
     *     {@link CE3CONTENT }
     *     
     */
    public CE3CONTENT getCE3() {
        return ce3;
    }

    /**
     * Sets the value of the ce3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CE3CONTENT }
     *     
     */
    public void setCE3(CE3CONTENT value) {
        this.ce3 = value;
    }

    /**
     * Gets the value of the ce4 property.
     * 
     * @return
     *     possible object is
     *     {@link CE4CONTENT }
     *     
     */
    public CE4CONTENT getCE4() {
        return ce4;
    }

    /**
     * Sets the value of the ce4 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CE4CONTENT }
     *     
     */
    public void setCE4(CE4CONTENT value) {
        this.ce4 = value;
    }

    /**
     * Gets the value of the ce5 property.
     * 
     * @return
     *     possible object is
     *     {@link CE5CONTENT }
     *     
     */
    public CE5CONTENT getCE5() {
        return ce5;
    }

    /**
     * Sets the value of the ce5 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CE5CONTENT }
     *     
     */
    public void setCE5(CE5CONTENT value) {
        this.ce5 = value;
    }

    /**
     * Gets the value of the ce6 property.
     * 
     * @return
     *     possible object is
     *     {@link CE6CONTENT }
     *     
     */
    public CE6CONTENT getCE6() {
        return ce6;
    }

    /**
     * Sets the value of the ce6 property.
     * 
     * @param value
     *     allowed object is
     *     {@link CE6CONTENT }
     *     
     */
    public void setCE6(CE6CONTENT value) {
        this.ce6 = value;
    }

}
