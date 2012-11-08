//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.1-b02-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.04.18 at 09:20:26 PM IST 
//


package com.sun.jbi.hl7bc.extservice.ack.hl7v24;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;


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
@XmlSeeAlso({
    MSH3CONTENT.class,
    MSH4CONTENT.class,
    MSH5CONTENT.class,
    MSH6CONTENT.class
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
