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
 * @(#)SFTCONTENT.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */


package com.sun.jbi.hl7bc.extservice.ack.hl7v25;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import org.w3c.dom.Element;


/**
 * <p>Java class for SFT.CONTENT complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="SFT.CONTENT">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}SFT.1"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}SFT.2"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}SFT.3"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}SFT.4"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}SFT.5" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}SFT.6" minOccurs="0"/>
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
@XmlType(name = "SFT.CONTENT", propOrder = {
    "sft1",
    "sft2",
    "sft3",
    "sft4",
    "sft5",
    "sft6",
    "any"
})
public class SFTCONTENT {

    @XmlElement(name = "SFT.1", required = true)
    protected SFT1CONTENT sft1;
    @XmlElement(name = "SFT.2", required = true)
    protected SFT2CONTENT sft2;
    @XmlElement(name = "SFT.3", required = true)
    protected SFT3CONTENT sft3;
    @XmlElement(name = "SFT.4", required = true)
    protected SFT4CONTENT sft4;
    @XmlElement(name = "SFT.5")
    protected SFT5CONTENT sft5;
    @XmlElement(name = "SFT.6")
    protected SFT6CONTENT sft6;
    @XmlAnyElement(lax = true)
    protected Object any;

    /**
     * Gets the value of the sft1 property.
     * 
     * @return
     *     possible object is
     *     {@link SFT1CONTENT }
     *     
     */
    public SFT1CONTENT getSFT1() {
        return sft1;
    }

    /**
     * Sets the value of the sft1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link SFT1CONTENT }
     *     
     */
    public void setSFT1(SFT1CONTENT value) {
        this.sft1 = value;
    }

    /**
     * Gets the value of the sft2 property.
     * 
     * @return
     *     possible object is
     *     {@link SFT2CONTENT }
     *     
     */
    public SFT2CONTENT getSFT2() {
        return sft2;
    }

    /**
     * Sets the value of the sft2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link SFT2CONTENT }
     *     
     */
    public void setSFT2(SFT2CONTENT value) {
        this.sft2 = value;
    }

    /**
     * Gets the value of the sft3 property.
     * 
     * @return
     *     possible object is
     *     {@link SFT3CONTENT }
     *     
     */
    public SFT3CONTENT getSFT3() {
        return sft3;
    }

    /**
     * Sets the value of the sft3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link SFT3CONTENT }
     *     
     */
    public void setSFT3(SFT3CONTENT value) {
        this.sft3 = value;
    }

    /**
     * Gets the value of the sft4 property.
     * 
     * @return
     *     possible object is
     *     {@link SFT4CONTENT }
     *     
     */
    public SFT4CONTENT getSFT4() {
        return sft4;
    }

    /**
     * Sets the value of the sft4 property.
     * 
     * @param value
     *     allowed object is
     *     {@link SFT4CONTENT }
     *     
     */
    public void setSFT4(SFT4CONTENT value) {
        this.sft4 = value;
    }

    /**
     * Gets the value of the sft5 property.
     * 
     * @return
     *     possible object is
     *     {@link SFT5CONTENT }
     *     
     */
    public SFT5CONTENT getSFT5() {
        return sft5;
    }

    /**
     * Sets the value of the sft5 property.
     * 
     * @param value
     *     allowed object is
     *     {@link SFT5CONTENT }
     *     
     */
    public void setSFT5(SFT5CONTENT value) {
        this.sft5 = value;
    }

    /**
     * Gets the value of the sft6 property.
     * 
     * @return
     *     possible object is
     *     {@link SFT6CONTENT }
     *     
     */
    public SFT6CONTENT getSFT6() {
        return sft6;
    }

    /**
     * Sets the value of the sft6 property.
     * 
     * @param value
     *     allowed object is
     *     {@link SFT6CONTENT }
     *     
     */
    public void setSFT6(SFT6CONTENT value) {
        this.sft6 = value;
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
