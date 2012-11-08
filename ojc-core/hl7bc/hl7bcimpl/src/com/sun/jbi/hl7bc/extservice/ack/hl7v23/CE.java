//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.1-b02-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.04.18 at 10:55:05 PM IST 
//


package com.sun.jbi.hl7bc.extservice.ack.hl7v23;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;


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
@XmlSeeAlso({
    MSA6CONTENT.class,
    MSH19CONTENT.class,
    CMELD4CONTENT.class
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