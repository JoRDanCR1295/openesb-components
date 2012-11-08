/*
 * @(#)Invoice.java        $Revision: 1.2 $ $Date: 2008/10/25 22:04:12 $
 * 
 * Copyright (c) 2008 Milan Fort (http://www.milanfort.com/). All rights reserved.
 * 
 * The contents of this file are subject to the terms of the Common Development
 * and Distribution License (the "License"). You may not use this file except
 * in compliance with the License.
 * 
 * You can obtain a copy of the license at http://www.sun.com/cddl/cddl.html.
 * See the License for the specific language governing permissions and limitations
 * under the License.
 */

package org.openesb.components.rules4jbi.test4rules.domain;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

/**
 * This class is similar to the one used within JSR 94 TCK as input/output business object.
 * We use it for various fuctional testing.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/10/25 22:04:12 $
 * 
 * @since 0.1
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlRootElement(name="bill", namespace="http://www.example.org/xml/ns/class")
@XmlType(name="billType", namespace="http://www.example.org/xml/ns/class")
public class Invoice {
    
    @XmlAttribute
    private int amount;
    
    @XmlElement(required=true, namespace="http://www.example.org/xml/ns/class")
    private String description;
    
    @XmlElement(required=true, namespace="http://www.example.org/xml/ns/class")
    private String status;

    // For direct use with JAXB, we need a no-arg constructor
    public Invoice() {}

    public Invoice(String description) {
        this.description = description;
        amount = 0;
        status = "unpaid";
    }
    
    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }
    
    public int getAmount() {
        return amount;
    }

    public void setAmount(int amount) {
        this.amount = amount;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    @Override
    public String toString() {
        return "Invoice; description: " + description + ", amount: $" + amount + ", status: " + status;
    }
    
    public static void main(String[] args) {
        Invoice invoice = new Invoice();
        
        invoice.setDescription("invoice for consulting services");
        invoice.setAmount(10000);
        invoice.setStatus("unpaid");
        
        System.out.println(invoice);
    }
}
