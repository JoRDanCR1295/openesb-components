 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.test.webservice.generator.testclasses;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;

@XmlAccessorType(XmlAccessType.PROPERTY)
public class UserProfile implements Serializable {
        
      
      public String name = null;
      public String address = null;
      public int age = (int)0;
      
      public int[] testArray;

      public UserProfile ()
      {
      } // ctor

      public UserProfile (String _name, String _address, int _age)
      {
        name = _name;
        address = _address;
        age = _age;
      } // ctor

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public int getAge() {
        return age;
    }

    public void setAge(int age) {
        this.age = age;
    }

	public int[] getTestArray() {
		return testArray;
	}

	public void setTestArray(int[] testArray) {
		this.testArray = testArray;
	}
      

} // class UserProfile
