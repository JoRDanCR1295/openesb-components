/*
 * UserProfile.java
 *
 * Created on 18 May 2007, 11:48
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package it.imolinfo.test14.complex;

import java.io.Serializable;
import java.util.Date;

/**
 *
 * @author marco
 */
public class UserProfile implements Serializable {
        
    
    private String name;
    
    private String address;
    
    private long age;
    
    // private Date birthday;
    
    /** Creates a new instance of UserProfile */
    public UserProfile() {}
    

  public UserProfile (String name, String address, int age) {
    this.name = name;
    this.address = address;
    this.age = age;
    // this.birthday = birthday;
    
  } // ctor    
    
    public void setName(String name) {
        this.name = name;
    }
    
    public void setAddress(String address) {
        this.address = address;
    }
    
    public void setAge(long age) {
        this.age = age;        
    }
    
    
    
    public String getName() {
        return name;
    }
    
    public String getAddress() {
        return address;
    }
    
    public long getAge() {
        return age;
    }    
    

    public String toString() {
        StringBuffer str = new StringBuffer();
        str.append("Name: " + name).append("\n")
            .append("Address: " + address).append("\n")
            .append("Age: " + age).append("\n");
            
        return str.toString();
    }
       
}
