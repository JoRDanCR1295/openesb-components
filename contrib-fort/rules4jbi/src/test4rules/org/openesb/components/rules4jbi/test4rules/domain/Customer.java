/*
 * @(#)Customer.java        $Revision: 1.2 $ $Date: 2008/10/25 22:04:12 $
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
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * This class is similar to the one used within JSR 94 TCK as input/output business object.
 * We use it for various fuctional testing.
 *
 * @author Milan Fort (http://www.milanfort.com/)
 * @version $Revision: 1.2 $ $Date: 2008/10/25 22:04:12 $
 * 
 * @since 0.1
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class Customer {
    
    @XmlElement(required=true)
    private String name;
    
    private int creditLimit;
    
    // For direct use with JAXB, we need a no-arg constructor
    public Customer() {}

    public Customer(String name) {
        this.name = name;
    }
    
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
    
    public int getCreditLimit() {
        return creditLimit;
    }

    public void setCreditLimit(int creditLimit) {
        this.creditLimit = creditLimit;
    }
    
    @Override
    public String toString() {
        return "Customer '" + name + "' with credit limit $" + creditLimit;
    }
    
    public static void main(String[] args) {
        Customer customer = new Customer();
        
        customer.setName("Johnny Bravo");
        customer.setCreditLimit(500);
        
        System.out.println(customer);
    }
}
