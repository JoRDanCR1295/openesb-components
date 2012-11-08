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
 * @(#)XTN.java 
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
 * <p>Java class for XTN complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="XTN">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}XTN.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}XTN.2" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}XTN.3" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}XTN.4" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}XTN.5" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}XTN.6" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}XTN.7" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}XTN.8" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}XTN.9" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}XTN.10" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}XTN.11" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}XTN.12" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "XTN", namespace = "urn:hl7-org:v2xml", propOrder = {
    "xtn1",
    "xtn2",
    "xtn3",
    "xtn4",
    "xtn5",
    "xtn6",
    "xtn7",
    "xtn8",
    "xtn9",
    "xtn10",
    "xtn11",
    "xtn12"
})
@XmlSeeAlso({
    ERR12CONTENT.class
})
public class XTN {

    @XmlElement(name = "XTN.1", namespace = "urn:hl7-org:v2xml")
    protected XTN1CONTENT xtn1;
    @XmlElement(name = "XTN.2", namespace = "urn:hl7-org:v2xml")
    protected XTN2CONTENT xtn2;
    @XmlElement(name = "XTN.3", namespace = "urn:hl7-org:v2xml")
    protected XTN3CONTENT xtn3;
    @XmlElement(name = "XTN.4", namespace = "urn:hl7-org:v2xml")
    protected XTN4CONTENT xtn4;
    @XmlElement(name = "XTN.5", namespace = "urn:hl7-org:v2xml")
    protected XTN5CONTENT xtn5;
    @XmlElement(name = "XTN.6", namespace = "urn:hl7-org:v2xml")
    protected XTN6CONTENT xtn6;
    @XmlElement(name = "XTN.7", namespace = "urn:hl7-org:v2xml")
    protected XTN7CONTENT xtn7;
    @XmlElement(name = "XTN.8", namespace = "urn:hl7-org:v2xml")
    protected XTN8CONTENT xtn8;
    @XmlElement(name = "XTN.9", namespace = "urn:hl7-org:v2xml")
    protected XTN9CONTENT xtn9;
    @XmlElement(name = "XTN.10", namespace = "urn:hl7-org:v2xml")
    protected XTN10CONTENT xtn10;
    @XmlElement(name = "XTN.11", namespace = "urn:hl7-org:v2xml")
    protected XTN11CONTENT xtn11;
    @XmlElement(name = "XTN.12", namespace = "urn:hl7-org:v2xml")
    protected XTN12CONTENT xtn12;

    /**
     * Gets the value of the xtn1 property.
     * 
     * @return
     *     possible object is
     *     {@link XTN1CONTENT }
     *     
     */
    public XTN1CONTENT getXTN1() {
        return xtn1;
    }

    /**
     * Sets the value of the xtn1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link XTN1CONTENT }
     *     
     */
    public void setXTN1(XTN1CONTENT value) {
        this.xtn1 = value;
    }

    /**
     * Gets the value of the xtn2 property.
     * 
     * @return
     *     possible object is
     *     {@link XTN2CONTENT }
     *     
     */
    public XTN2CONTENT getXTN2() {
        return xtn2;
    }

    /**
     * Sets the value of the xtn2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link XTN2CONTENT }
     *     
     */
    public void setXTN2(XTN2CONTENT value) {
        this.xtn2 = value;
    }

    /**
     * Gets the value of the xtn3 property.
     * 
     * @return
     *     possible object is
     *     {@link XTN3CONTENT }
     *     
     */
    public XTN3CONTENT getXTN3() {
        return xtn3;
    }

    /**
     * Sets the value of the xtn3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link XTN3CONTENT }
     *     
     */
    public void setXTN3(XTN3CONTENT value) {
        this.xtn3 = value;
    }

    /**
     * Gets the value of the xtn4 property.
     * 
     * @return
     *     possible object is
     *     {@link XTN4CONTENT }
     *     
     */
    public XTN4CONTENT getXTN4() {
        return xtn4;
    }

    /**
     * Sets the value of the xtn4 property.
     * 
     * @param value
     *     allowed object is
     *     {@link XTN4CONTENT }
     *     
     */
    public void setXTN4(XTN4CONTENT value) {
        this.xtn4 = value;
    }

    /**
     * Gets the value of the xtn5 property.
     * 
     * @return
     *     possible object is
     *     {@link XTN5CONTENT }
     *     
     */
    public XTN5CONTENT getXTN5() {
        return xtn5;
    }

    /**
     * Sets the value of the xtn5 property.
     * 
     * @param value
     *     allowed object is
     *     {@link XTN5CONTENT }
     *     
     */
    public void setXTN5(XTN5CONTENT value) {
        this.xtn5 = value;
    }

    /**
     * Gets the value of the xtn6 property.
     * 
     * @return
     *     possible object is
     *     {@link XTN6CONTENT }
     *     
     */
    public XTN6CONTENT getXTN6() {
        return xtn6;
    }

    /**
     * Sets the value of the xtn6 property.
     * 
     * @param value
     *     allowed object is
     *     {@link XTN6CONTENT }
     *     
     */
    public void setXTN6(XTN6CONTENT value) {
        this.xtn6 = value;
    }

    /**
     * Gets the value of the xtn7 property.
     * 
     * @return
     *     possible object is
     *     {@link XTN7CONTENT }
     *     
     */
    public XTN7CONTENT getXTN7() {
        return xtn7;
    }

    /**
     * Sets the value of the xtn7 property.
     * 
     * @param value
     *     allowed object is
     *     {@link XTN7CONTENT }
     *     
     */
    public void setXTN7(XTN7CONTENT value) {
        this.xtn7 = value;
    }

    /**
     * Gets the value of the xtn8 property.
     * 
     * @return
     *     possible object is
     *     {@link XTN8CONTENT }
     *     
     */
    public XTN8CONTENT getXTN8() {
        return xtn8;
    }

    /**
     * Sets the value of the xtn8 property.
     * 
     * @param value
     *     allowed object is
     *     {@link XTN8CONTENT }
     *     
     */
    public void setXTN8(XTN8CONTENT value) {
        this.xtn8 = value;
    }

    /**
     * Gets the value of the xtn9 property.
     * 
     * @return
     *     possible object is
     *     {@link XTN9CONTENT }
     *     
     */
    public XTN9CONTENT getXTN9() {
        return xtn9;
    }

    /**
     * Sets the value of the xtn9 property.
     * 
     * @param value
     *     allowed object is
     *     {@link XTN9CONTENT }
     *     
     */
    public void setXTN9(XTN9CONTENT value) {
        this.xtn9 = value;
    }

    /**
     * Gets the value of the xtn10 property.
     * 
     * @return
     *     possible object is
     *     {@link XTN10CONTENT }
     *     
     */
    public XTN10CONTENT getXTN10() {
        return xtn10;
    }

    /**
     * Sets the value of the xtn10 property.
     * 
     * @param value
     *     allowed object is
     *     {@link XTN10CONTENT }
     *     
     */
    public void setXTN10(XTN10CONTENT value) {
        this.xtn10 = value;
    }

    /**
     * Gets the value of the xtn11 property.
     * 
     * @return
     *     possible object is
     *     {@link XTN11CONTENT }
     *     
     */
    public XTN11CONTENT getXTN11() {
        return xtn11;
    }

    /**
     * Sets the value of the xtn11 property.
     * 
     * @param value
     *     allowed object is
     *     {@link XTN11CONTENT }
     *     
     */
    public void setXTN11(XTN11CONTENT value) {
        this.xtn11 = value;
    }

    /**
     * Gets the value of the xtn12 property.
     * 
     * @return
     *     possible object is
     *     {@link XTN12CONTENT }
     *     
     */
    public XTN12CONTENT getXTN12() {
        return xtn12;
    }

    /**
     * Sets the value of the xtn12 property.
     * 
     * @param value
     *     allowed object is
     *     {@link XTN12CONTENT }
     *     
     */
    public void setXTN12(XTN12CONTENT value) {
        this.xtn12 = value;
    }

}
