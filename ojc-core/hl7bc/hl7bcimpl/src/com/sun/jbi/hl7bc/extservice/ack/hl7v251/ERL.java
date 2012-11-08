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
 * @(#)ERL.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */



package com.sun.jbi.hl7bc.extservice.ack.hl7v251;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for ERL complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ERL">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}ERL.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}ERL.2" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}ERL.3" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}ERL.4" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}ERL.5" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}ERL.6" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "ERL", namespace = "urn:hl7-org:v2xml", propOrder = {
    "erl1",
    "erl2",
    "erl3",
    "erl4",
    "erl5",
    "erl6"
})
@XmlSeeAlso({
    ERR2CONTENT.class
})
public class ERL {

    @XmlElement(name = "ERL.1", namespace = "urn:hl7-org:v2xml")
    protected ERL1CONTENT erl1;
    @XmlElement(name = "ERL.2", namespace = "urn:hl7-org:v2xml")
    protected ERL2CONTENT erl2;
    @XmlElement(name = "ERL.3", namespace = "urn:hl7-org:v2xml")
    protected ERL3CONTENT erl3;
    @XmlElement(name = "ERL.4", namespace = "urn:hl7-org:v2xml")
    protected ERL4CONTENT erl4;
    @XmlElement(name = "ERL.5", namespace = "urn:hl7-org:v2xml")
    protected ERL5CONTENT erl5;
    @XmlElement(name = "ERL.6", namespace = "urn:hl7-org:v2xml")
    protected ERL6CONTENT erl6;

    /**
     * Gets the value of the erl1 property.
     * 
     * @return
     *     possible object is
     *     {@link ERL1CONTENT }
     *     
     */
    public ERL1CONTENT getERL1() {
        return erl1;
    }

    /**
     * Sets the value of the erl1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link ERL1CONTENT }
     *     
     */
    public void setERL1(ERL1CONTENT value) {
        this.erl1 = value;
    }

    /**
     * Gets the value of the erl2 property.
     * 
     * @return
     *     possible object is
     *     {@link ERL2CONTENT }
     *     
     */
    public ERL2CONTENT getERL2() {
        return erl2;
    }

    /**
     * Sets the value of the erl2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link ERL2CONTENT }
     *     
     */
    public void setERL2(ERL2CONTENT value) {
        this.erl2 = value;
    }

    /**
     * Gets the value of the erl3 property.
     * 
     * @return
     *     possible object is
     *     {@link ERL3CONTENT }
     *     
     */
    public ERL3CONTENT getERL3() {
        return erl3;
    }

    /**
     * Sets the value of the erl3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link ERL3CONTENT }
     *     
     */
    public void setERL3(ERL3CONTENT value) {
        this.erl3 = value;
    }

    /**
     * Gets the value of the erl4 property.
     * 
     * @return
     *     possible object is
     *     {@link ERL4CONTENT }
     *     
     */
    public ERL4CONTENT getERL4() {
        return erl4;
    }

    /**
     * Sets the value of the erl4 property.
     * 
     * @param value
     *     allowed object is
     *     {@link ERL4CONTENT }
     *     
     */
    public void setERL4(ERL4CONTENT value) {
        this.erl4 = value;
    }

    /**
     * Gets the value of the erl5 property.
     * 
     * @return
     *     possible object is
     *     {@link ERL5CONTENT }
     *     
     */
    public ERL5CONTENT getERL5() {
        return erl5;
    }

    /**
     * Sets the value of the erl5 property.
     * 
     * @param value
     *     allowed object is
     *     {@link ERL5CONTENT }
     *     
     */
    public void setERL5(ERL5CONTENT value) {
        this.erl5 = value;
    }

    /**
     * Gets the value of the erl6 property.
     * 
     * @return
     *     possible object is
     *     {@link ERL6CONTENT }
     *     
     */
    public ERL6CONTENT getERL6() {
        return erl6;
    }

    /**
     * Sets the value of the erl6 property.
     * 
     * @param value
     *     allowed object is
     *     {@link ERL6CONTENT }
     *     
     */
    public void setERL6(ERL6CONTENT value) {
        this.erl6 = value;
    }

}
