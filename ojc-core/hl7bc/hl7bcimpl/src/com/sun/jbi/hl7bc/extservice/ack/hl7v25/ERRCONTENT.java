//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.1-b02-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.01.22 at 07:55:12 PM IST 
//


package com.sun.jbi.hl7bc.extservice.ack.hl7v25;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAnyElement;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;
import org.w3c.dom.Element;


/**
 * <p>Java class for ERR.CONTENT complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="ERR.CONTENT">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}ERR.1" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}ERR.2" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}ERR.3"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}ERR.4"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}ERR.5" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}ERR.6" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}ERR.7" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}ERR.8" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}ERR.9" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}ERR.10" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}ERR.11" maxOccurs="unbounded" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}ERR.12" maxOccurs="unbounded" minOccurs="0"/>
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
@XmlType(name = "ERR.CONTENT", propOrder = {
    "err1",
    "err2",
    "err3",
    "err4",
    "err5",
    "err6",
    "err7",
    "err8",
    "err9",
    "err10",
    "err11",
    "err12",
    "any"
})
public class ERRCONTENT {

    @XmlElement(name = "ERR.1")
    protected List<ERR1CONTENT> err1;
    @XmlElement(name = "ERR.2")
    protected List<ERR2CONTENT> err2;
    @XmlElement(name = "ERR.3", required = true)
    protected ERR3CONTENT err3;
    @XmlElement(name = "ERR.4", required = true)
    protected ERR4CONTENT err4;
    @XmlElement(name = "ERR.5")
    protected ERR5CONTENT err5;
    @XmlElement(name = "ERR.6")
    protected List<ERR6CONTENT> err6;
    @XmlElement(name = "ERR.7")
    protected ERR7CONTENT err7;
    @XmlElement(name = "ERR.8")
    protected ERR8CONTENT err8;
    @XmlElement(name = "ERR.9")
    protected List<ERR9CONTENT> err9;
    @XmlElement(name = "ERR.10")
    protected ERR10CONTENT err10;
    @XmlElement(name = "ERR.11")
    protected List<ERR11CONTENT> err11;
    @XmlElement(name = "ERR.12")
    protected List<ERR12CONTENT> err12;
    @XmlAnyElement(lax = true)
    protected Object any;

    /**
     * Gets the value of the err1 property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the err1 property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getERR1().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ERR1CONTENT }
     * 
     * 
     */
    public List<ERR1CONTENT> getERR1() {
        if (err1 == null) {
            err1 = new ArrayList<ERR1CONTENT>();
        }
        return this.err1;
    }

    /**
     * Gets the value of the err2 property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the err2 property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getERR2().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ERR2CONTENT }
     * 
     * 
     */
    public List<ERR2CONTENT> getERR2() {
        if (err2 == null) {
            err2 = new ArrayList<ERR2CONTENT>();
        }
        return this.err2;
    }

    /**
     * Gets the value of the err3 property.
     * 
     * @return
     *     possible object is
     *     {@link ERR3CONTENT }
     *     
     */
    public ERR3CONTENT getERR3() {
        return err3;
    }

    /**
     * Sets the value of the err3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link ERR3CONTENT }
     *     
     */
    public void setERR3(ERR3CONTENT value) {
        this.err3 = value;
    }

    /**
     * Gets the value of the err4 property.
     * 
     * @return
     *     possible object is
     *     {@link ERR4CONTENT }
     *     
     */
    public ERR4CONTENT getERR4() {
        return err4;
    }

    /**
     * Sets the value of the err4 property.
     * 
     * @param value
     *     allowed object is
     *     {@link ERR4CONTENT }
     *     
     */
    public void setERR4(ERR4CONTENT value) {
        this.err4 = value;
    }

    /**
     * Gets the value of the err5 property.
     * 
     * @return
     *     possible object is
     *     {@link ERR5CONTENT }
     *     
     */
    public ERR5CONTENT getERR5() {
        return err5;
    }

    /**
     * Sets the value of the err5 property.
     * 
     * @param value
     *     allowed object is
     *     {@link ERR5CONTENT }
     *     
     */
    public void setERR5(ERR5CONTENT value) {
        this.err5 = value;
    }

    /**
     * Gets the value of the err6 property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the err6 property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getERR6().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ERR6CONTENT }
     * 
     * 
     */
    public List<ERR6CONTENT> getERR6() {
        if (err6 == null) {
            err6 = new ArrayList<ERR6CONTENT>();
        }
        return this.err6;
    }

    /**
     * Gets the value of the err7 property.
     * 
     * @return
     *     possible object is
     *     {@link ERR7CONTENT }
     *     
     */
    public ERR7CONTENT getERR7() {
        return err7;
    }

    /**
     * Sets the value of the err7 property.
     * 
     * @param value
     *     allowed object is
     *     {@link ERR7CONTENT }
     *     
     */
    public void setERR7(ERR7CONTENT value) {
        this.err7 = value;
    }

    /**
     * Gets the value of the err8 property.
     * 
     * @return
     *     possible object is
     *     {@link ERR8CONTENT }
     *     
     */
    public ERR8CONTENT getERR8() {
        return err8;
    }

    /**
     * Sets the value of the err8 property.
     * 
     * @param value
     *     allowed object is
     *     {@link ERR8CONTENT }
     *     
     */
    public void setERR8(ERR8CONTENT value) {
        this.err8 = value;
    }

    /**
     * Gets the value of the err9 property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the err9 property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getERR9().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ERR9CONTENT }
     * 
     * 
     */
    public List<ERR9CONTENT> getERR9() {
        if (err9 == null) {
            err9 = new ArrayList<ERR9CONTENT>();
        }
        return this.err9;
    }

    /**
     * Gets the value of the err10 property.
     * 
     * @return
     *     possible object is
     *     {@link ERR10CONTENT }
     *     
     */
    public ERR10CONTENT getERR10() {
        return err10;
    }

    /**
     * Sets the value of the err10 property.
     * 
     * @param value
     *     allowed object is
     *     {@link ERR10CONTENT }
     *     
     */
    public void setERR10(ERR10CONTENT value) {
        this.err10 = value;
    }

    /**
     * Gets the value of the err11 property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the err11 property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getERR11().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ERR11CONTENT }
     * 
     * 
     */
    public List<ERR11CONTENT> getERR11() {
        if (err11 == null) {
            err11 = new ArrayList<ERR11CONTENT>();
        }
        return this.err11;
    }

    /**
     * Gets the value of the err12 property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the err12 property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getERR12().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link ERR12CONTENT }
     * 
     * 
     */
    public List<ERR12CONTENT> getERR12() {
        if (err12 == null) {
            err12 = new ArrayList<ERR12CONTENT>();
        }
        return this.err12;
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
