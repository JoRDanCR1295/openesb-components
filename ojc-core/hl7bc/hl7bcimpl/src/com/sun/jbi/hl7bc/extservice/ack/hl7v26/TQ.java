//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.1-b02-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2008.12.04 at 12:45:30 AM IST 
//


package com.sun.jbi.hl7bc.extservice.ack.hl7v26;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for TQ complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="TQ">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}TQ.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}TQ.2" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}TQ.3" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}TQ.4" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}TQ.5" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}TQ.6" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}TQ.7" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}TQ.8" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}TQ.9" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}TQ.10" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}TQ.11" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}TQ.12" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "TQ", propOrder = {
    "tq1",
    "tq2",
    "tq3",
    "tq4",
    "tq5",
    "tq6",
    "tq7",
    "tq8",
    "tq9",
    "tq10",
    "tq11",
    "tq12"
})
public class TQ {

    @XmlElement(name = "TQ.1")
    protected TQ1CONTENT tq1;
    @XmlElement(name = "TQ.2")
    protected TQ2CONTENT tq2;
    @XmlElement(name = "TQ.3")
    protected TQ3CONTENT tq3;
    @XmlElement(name = "TQ.4")
    protected TQ4CONTENT tq4;
    @XmlElement(name = "TQ.5")
    protected TQ5CONTENT tq5;
    @XmlElement(name = "TQ.6")
    protected TQ6CONTENT tq6;
    @XmlElement(name = "TQ.7")
    protected TQ7CONTENT tq7;
    @XmlElement(name = "TQ.8")
    protected TQ8CONTENT tq8;
    @XmlElement(name = "TQ.9")
    protected TQ9CONTENT tq9;
    @XmlElement(name = "TQ.10")
    protected TQ10CONTENT tq10;
    @XmlElement(name = "TQ.11")
    protected TQ11CONTENT tq11;
    @XmlElement(name = "TQ.12")
    protected TQ12CONTENT tq12;

    /**
     * Gets the value of the tq1 property.
     * 
     * @return
     *     possible object is
     *     {@link TQ1CONTENT }
     *     
     */
    public TQ1CONTENT getTQ1() {
        return tq1;
    }

    /**
     * Sets the value of the tq1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link TQ1CONTENT }
     *     
     */
    public void setTQ1(TQ1CONTENT value) {
        this.tq1 = value;
    }

    /**
     * Gets the value of the tq2 property.
     * 
     * @return
     *     possible object is
     *     {@link TQ2CONTENT }
     *     
     */
    public TQ2CONTENT getTQ2() {
        return tq2;
    }

    /**
     * Sets the value of the tq2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link TQ2CONTENT }
     *     
     */
    public void setTQ2(TQ2CONTENT value) {
        this.tq2 = value;
    }

    /**
     * Gets the value of the tq3 property.
     * 
     * @return
     *     possible object is
     *     {@link TQ3CONTENT }
     *     
     */
    public TQ3CONTENT getTQ3() {
        return tq3;
    }

    /**
     * Sets the value of the tq3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link TQ3CONTENT }
     *     
     */
    public void setTQ3(TQ3CONTENT value) {
        this.tq3 = value;
    }

    /**
     * Gets the value of the tq4 property.
     * 
     * @return
     *     possible object is
     *     {@link TQ4CONTENT }
     *     
     */
    public TQ4CONTENT getTQ4() {
        return tq4;
    }

    /**
     * Sets the value of the tq4 property.
     * 
     * @param value
     *     allowed object is
     *     {@link TQ4CONTENT }
     *     
     */
    public void setTQ4(TQ4CONTENT value) {
        this.tq4 = value;
    }

    /**
     * Gets the value of the tq5 property.
     * 
     * @return
     *     possible object is
     *     {@link TQ5CONTENT }
     *     
     */
    public TQ5CONTENT getTQ5() {
        return tq5;
    }

    /**
     * Sets the value of the tq5 property.
     * 
     * @param value
     *     allowed object is
     *     {@link TQ5CONTENT }
     *     
     */
    public void setTQ5(TQ5CONTENT value) {
        this.tq5 = value;
    }

    /**
     * Gets the value of the tq6 property.
     * 
     * @return
     *     possible object is
     *     {@link TQ6CONTENT }
     *     
     */
    public TQ6CONTENT getTQ6() {
        return tq6;
    }

    /**
     * Sets the value of the tq6 property.
     * 
     * @param value
     *     allowed object is
     *     {@link TQ6CONTENT }
     *     
     */
    public void setTQ6(TQ6CONTENT value) {
        this.tq6 = value;
    }

    /**
     * Gets the value of the tq7 property.
     * 
     * @return
     *     possible object is
     *     {@link TQ7CONTENT }
     *     
     */
    public TQ7CONTENT getTQ7() {
        return tq7;
    }

    /**
     * Sets the value of the tq7 property.
     * 
     * @param value
     *     allowed object is
     *     {@link TQ7CONTENT }
     *     
     */
    public void setTQ7(TQ7CONTENT value) {
        this.tq7 = value;
    }

    /**
     * Gets the value of the tq8 property.
     * 
     * @return
     *     possible object is
     *     {@link TQ8CONTENT }
     *     
     */
    public TQ8CONTENT getTQ8() {
        return tq8;
    }

    /**
     * Sets the value of the tq8 property.
     * 
     * @param value
     *     allowed object is
     *     {@link TQ8CONTENT }
     *     
     */
    public void setTQ8(TQ8CONTENT value) {
        this.tq8 = value;
    }

    /**
     * Gets the value of the tq9 property.
     * 
     * @return
     *     possible object is
     *     {@link TQ9CONTENT }
     *     
     */
    public TQ9CONTENT getTQ9() {
        return tq9;
    }

    /**
     * Sets the value of the tq9 property.
     * 
     * @param value
     *     allowed object is
     *     {@link TQ9CONTENT }
     *     
     */
    public void setTQ9(TQ9CONTENT value) {
        this.tq9 = value;
    }

    /**
     * Gets the value of the tq10 property.
     * 
     * @return
     *     possible object is
     *     {@link TQ10CONTENT }
     *     
     */
    public TQ10CONTENT getTQ10() {
        return tq10;
    }

    /**
     * Sets the value of the tq10 property.
     * 
     * @param value
     *     allowed object is
     *     {@link TQ10CONTENT }
     *     
     */
    public void setTQ10(TQ10CONTENT value) {
        this.tq10 = value;
    }

    /**
     * Gets the value of the tq11 property.
     * 
     * @return
     *     possible object is
     *     {@link TQ11CONTENT }
     *     
     */
    public TQ11CONTENT getTQ11() {
        return tq11;
    }

    /**
     * Sets the value of the tq11 property.
     * 
     * @param value
     *     allowed object is
     *     {@link TQ11CONTENT }
     *     
     */
    public void setTQ11(TQ11CONTENT value) {
        this.tq11 = value;
    }

    /**
     * Gets the value of the tq12 property.
     * 
     * @return
     *     possible object is
     *     {@link TQ12CONTENT }
     *     
     */
    public TQ12CONTENT getTQ12() {
        return tq12;
    }

    /**
     * Sets the value of the tq12 property.
     * 
     * @param value
     *     allowed object is
     *     {@link TQ12CONTENT }
     *     
     */
    public void setTQ12(TQ12CONTENT value) {
        this.tq12 = value;
    }

}
