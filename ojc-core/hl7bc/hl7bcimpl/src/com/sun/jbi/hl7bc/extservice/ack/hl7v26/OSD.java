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
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for OSD complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="OSD">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}OSD.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}OSD.2" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}OSD.3" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}OSD.4" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}OSD.5" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}OSD.6" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}OSD.7" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}OSD.8" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}OSD.9" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}OSD.10" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}OSD.11" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "OSD", propOrder = {
    "osd1",
    "osd2",
    "osd3",
    "osd4",
    "osd5",
    "osd6",
    "osd7",
    "osd8",
    "osd9",
    "osd10",
    "osd11"
})
@XmlSeeAlso({
    TQ10CONTENT.class
})
public class OSD {

    @XmlElement(name = "OSD.1")
    protected OSD1CONTENT osd1;
    @XmlElement(name = "OSD.2")
    protected OSD2CONTENT osd2;
    @XmlElement(name = "OSD.3")
    protected OSD3CONTENT osd3;
    @XmlElement(name = "OSD.4")
    protected OSD4CONTENT osd4;
    @XmlElement(name = "OSD.5")
    protected OSD5CONTENT osd5;
    @XmlElement(name = "OSD.6")
    protected OSD6CONTENT osd6;
    @XmlElement(name = "OSD.7")
    protected OSD7CONTENT osd7;
    @XmlElement(name = "OSD.8")
    protected OSD8CONTENT osd8;
    @XmlElement(name = "OSD.9")
    protected OSD9CONTENT osd9;
    @XmlElement(name = "OSD.10")
    protected OSD10CONTENT osd10;
    @XmlElement(name = "OSD.11")
    protected OSD11CONTENT osd11;

    /**
     * Gets the value of the osd1 property.
     * 
     * @return
     *     possible object is
     *     {@link OSD1CONTENT }
     *     
     */
    public OSD1CONTENT getOSD1() {
        return osd1;
    }

    /**
     * Sets the value of the osd1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link OSD1CONTENT }
     *     
     */
    public void setOSD1(OSD1CONTENT value) {
        this.osd1 = value;
    }

    /**
     * Gets the value of the osd2 property.
     * 
     * @return
     *     possible object is
     *     {@link OSD2CONTENT }
     *     
     */
    public OSD2CONTENT getOSD2() {
        return osd2;
    }

    /**
     * Sets the value of the osd2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link OSD2CONTENT }
     *     
     */
    public void setOSD2(OSD2CONTENT value) {
        this.osd2 = value;
    }

    /**
     * Gets the value of the osd3 property.
     * 
     * @return
     *     possible object is
     *     {@link OSD3CONTENT }
     *     
     */
    public OSD3CONTENT getOSD3() {
        return osd3;
    }

    /**
     * Sets the value of the osd3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link OSD3CONTENT }
     *     
     */
    public void setOSD3(OSD3CONTENT value) {
        this.osd3 = value;
    }

    /**
     * Gets the value of the osd4 property.
     * 
     * @return
     *     possible object is
     *     {@link OSD4CONTENT }
     *     
     */
    public OSD4CONTENT getOSD4() {
        return osd4;
    }

    /**
     * Sets the value of the osd4 property.
     * 
     * @param value
     *     allowed object is
     *     {@link OSD4CONTENT }
     *     
     */
    public void setOSD4(OSD4CONTENT value) {
        this.osd4 = value;
    }

    /**
     * Gets the value of the osd5 property.
     * 
     * @return
     *     possible object is
     *     {@link OSD5CONTENT }
     *     
     */
    public OSD5CONTENT getOSD5() {
        return osd5;
    }

    /**
     * Sets the value of the osd5 property.
     * 
     * @param value
     *     allowed object is
     *     {@link OSD5CONTENT }
     *     
     */
    public void setOSD5(OSD5CONTENT value) {
        this.osd5 = value;
    }

    /**
     * Gets the value of the osd6 property.
     * 
     * @return
     *     possible object is
     *     {@link OSD6CONTENT }
     *     
     */
    public OSD6CONTENT getOSD6() {
        return osd6;
    }

    /**
     * Sets the value of the osd6 property.
     * 
     * @param value
     *     allowed object is
     *     {@link OSD6CONTENT }
     *     
     */
    public void setOSD6(OSD6CONTENT value) {
        this.osd6 = value;
    }

    /**
     * Gets the value of the osd7 property.
     * 
     * @return
     *     possible object is
     *     {@link OSD7CONTENT }
     *     
     */
    public OSD7CONTENT getOSD7() {
        return osd7;
    }

    /**
     * Sets the value of the osd7 property.
     * 
     * @param value
     *     allowed object is
     *     {@link OSD7CONTENT }
     *     
     */
    public void setOSD7(OSD7CONTENT value) {
        this.osd7 = value;
    }

    /**
     * Gets the value of the osd8 property.
     * 
     * @return
     *     possible object is
     *     {@link OSD8CONTENT }
     *     
     */
    public OSD8CONTENT getOSD8() {
        return osd8;
    }

    /**
     * Sets the value of the osd8 property.
     * 
     * @param value
     *     allowed object is
     *     {@link OSD8CONTENT }
     *     
     */
    public void setOSD8(OSD8CONTENT value) {
        this.osd8 = value;
    }

    /**
     * Gets the value of the osd9 property.
     * 
     * @return
     *     possible object is
     *     {@link OSD9CONTENT }
     *     
     */
    public OSD9CONTENT getOSD9() {
        return osd9;
    }

    /**
     * Sets the value of the osd9 property.
     * 
     * @param value
     *     allowed object is
     *     {@link OSD9CONTENT }
     *     
     */
    public void setOSD9(OSD9CONTENT value) {
        this.osd9 = value;
    }

    /**
     * Gets the value of the osd10 property.
     * 
     * @return
     *     possible object is
     *     {@link OSD10CONTENT }
     *     
     */
    public OSD10CONTENT getOSD10() {
        return osd10;
    }

    /**
     * Sets the value of the osd10 property.
     * 
     * @param value
     *     allowed object is
     *     {@link OSD10CONTENT }
     *     
     */
    public void setOSD10(OSD10CONTENT value) {
        this.osd10 = value;
    }

    /**
     * Gets the value of the osd11 property.
     * 
     * @return
     *     possible object is
     *     {@link OSD11CONTENT }
     *     
     */
    public OSD11CONTENT getOSD11() {
        return osd11;
    }

    /**
     * Sets the value of the osd11 property.
     * 
     * @param value
     *     allowed object is
     *     {@link OSD11CONTENT }
     *     
     */
    public void setOSD11(OSD11CONTENT value) {
        this.osd11 = value;
    }

}