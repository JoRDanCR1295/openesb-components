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
 * @(#)XON.java 
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
 * <p>Java class for XON complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="XON">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}XON.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}XON.2" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}XON.3" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}XON.4" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}XON.5" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}XON.6" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}XON.7" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}XON.8" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}XON.9" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}XON.10" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "XON", namespace = "urn:hl7-org:v2xml", propOrder = {
    "xon1",
    "xon2",
    "xon3",
    "xon4",
    "xon5",
    "xon6",
    "xon7",
    "xon8",
    "xon9",
    "xon10"
})
@XmlSeeAlso({
    SFT1CONTENT.class
})
public class XON {

    @XmlElement(name = "XON.1", namespace = "urn:hl7-org:v2xml")
    protected XON1CONTENT xon1;
    @XmlElement(name = "XON.2", namespace = "urn:hl7-org:v2xml")
    protected XON2CONTENT xon2;
    @XmlElement(name = "XON.3", namespace = "urn:hl7-org:v2xml")
    protected XON3CONTENT xon3;
    @XmlElement(name = "XON.4", namespace = "urn:hl7-org:v2xml")
    protected XON4CONTENT xon4;
    @XmlElement(name = "XON.5", namespace = "urn:hl7-org:v2xml")
    protected XON5CONTENT xon5;
    @XmlElement(name = "XON.6", namespace = "urn:hl7-org:v2xml")
    protected XON6CONTENT xon6;
    @XmlElement(name = "XON.7", namespace = "urn:hl7-org:v2xml")
    protected XON7CONTENT xon7;
    @XmlElement(name = "XON.8", namespace = "urn:hl7-org:v2xml")
    protected XON8CONTENT xon8;
    @XmlElement(name = "XON.9", namespace = "urn:hl7-org:v2xml")
    protected XON9CONTENT xon9;
    @XmlElement(name = "XON.10", namespace = "urn:hl7-org:v2xml")
    protected XON10CONTENT xon10;

    /**
     * Gets the value of the xon1 property.
     * 
     * @return
     *     possible object is
     *     {@link XON1CONTENT }
     *     
     */
    public XON1CONTENT getXON1() {
        return xon1;
    }

    /**
     * Sets the value of the xon1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link XON1CONTENT }
     *     
     */
    public void setXON1(XON1CONTENT value) {
        this.xon1 = value;
    }

    /**
     * Gets the value of the xon2 property.
     * 
     * @return
     *     possible object is
     *     {@link XON2CONTENT }
     *     
     */
    public XON2CONTENT getXON2() {
        return xon2;
    }

    /**
     * Sets the value of the xon2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link XON2CONTENT }
     *     
     */
    public void setXON2(XON2CONTENT value) {
        this.xon2 = value;
    }

    /**
     * Gets the value of the xon3 property.
     * 
     * @return
     *     possible object is
     *     {@link XON3CONTENT }
     *     
     */
    public XON3CONTENT getXON3() {
        return xon3;
    }

    /**
     * Sets the value of the xon3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link XON3CONTENT }
     *     
     */
    public void setXON3(XON3CONTENT value) {
        this.xon3 = value;
    }

    /**
     * Gets the value of the xon4 property.
     * 
     * @return
     *     possible object is
     *     {@link XON4CONTENT }
     *     
     */
    public XON4CONTENT getXON4() {
        return xon4;
    }

    /**
     * Sets the value of the xon4 property.
     * 
     * @param value
     *     allowed object is
     *     {@link XON4CONTENT }
     *     
     */
    public void setXON4(XON4CONTENT value) {
        this.xon4 = value;
    }

    /**
     * Gets the value of the xon5 property.
     * 
     * @return
     *     possible object is
     *     {@link XON5CONTENT }
     *     
     */
    public XON5CONTENT getXON5() {
        return xon5;
    }

    /**
     * Sets the value of the xon5 property.
     * 
     * @param value
     *     allowed object is
     *     {@link XON5CONTENT }
     *     
     */
    public void setXON5(XON5CONTENT value) {
        this.xon5 = value;
    }

    /**
     * Gets the value of the xon6 property.
     * 
     * @return
     *     possible object is
     *     {@link XON6CONTENT }
     *     
     */
    public XON6CONTENT getXON6() {
        return xon6;
    }

    /**
     * Sets the value of the xon6 property.
     * 
     * @param value
     *     allowed object is
     *     {@link XON6CONTENT }
     *     
     */
    public void setXON6(XON6CONTENT value) {
        this.xon6 = value;
    }

    /**
     * Gets the value of the xon7 property.
     * 
     * @return
     *     possible object is
     *     {@link XON7CONTENT }
     *     
     */
    public XON7CONTENT getXON7() {
        return xon7;
    }

    /**
     * Sets the value of the xon7 property.
     * 
     * @param value
     *     allowed object is
     *     {@link XON7CONTENT }
     *     
     */
    public void setXON7(XON7CONTENT value) {
        this.xon7 = value;
    }

    /**
     * Gets the value of the xon8 property.
     * 
     * @return
     *     possible object is
     *     {@link XON8CONTENT }
     *     
     */
    public XON8CONTENT getXON8() {
        return xon8;
    }

    /**
     * Sets the value of the xon8 property.
     * 
     * @param value
     *     allowed object is
     *     {@link XON8CONTENT }
     *     
     */
    public void setXON8(XON8CONTENT value) {
        this.xon8 = value;
    }

    /**
     * Gets the value of the xon9 property.
     * 
     * @return
     *     possible object is
     *     {@link XON9CONTENT }
     *     
     */
    public XON9CONTENT getXON9() {
        return xon9;
    }

    /**
     * Sets the value of the xon9 property.
     * 
     * @param value
     *     allowed object is
     *     {@link XON9CONTENT }
     *     
     */
    public void setXON9(XON9CONTENT value) {
        this.xon9 = value;
    }

    /**
     * Gets the value of the xon10 property.
     * 
     * @return
     *     possible object is
     *     {@link XON10CONTENT }
     *     
     */
    public XON10CONTENT getXON10() {
        return xon10;
    }

    /**
     * Sets the value of the xon10 property.
     * 
     * @param value
     *     allowed object is
     *     {@link XON10CONTENT }
     *     
     */
    public void setXON10(XON10CONTENT value) {
        this.xon10 = value;
    }

}
