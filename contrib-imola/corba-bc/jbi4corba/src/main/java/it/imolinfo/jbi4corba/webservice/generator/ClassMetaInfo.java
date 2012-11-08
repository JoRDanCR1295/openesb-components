 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/
package it.imolinfo.jbi4corba.webservice.generator;

import java.util.HashSet;
import java.util.Set;

/**
 * The meta information of a class.
 */
public class ClassMetaInfo {

	/**
	 * XXX javadoc.
	 */
  protected String className = null;
  
  /**
   * XXX Javadoc.
   */
  protected String superClassName = null;

  /**
   * XXX Javadoc.
   */
  protected Set<String> interfaces = new HashSet<String>();

  /**
   * XXX Javadoc.
   */
  protected boolean serializable = false;
  
  /**
   * XXX Javadoc.
   */
  protected Long classSerialVersionUid = null;

  /**
   * Constructor.
   */
  public ClassMetaInfo() {
    // NOP
  }

  // getter and setter

  
  /**
   * @return  The return
   */
  public String getClassName() {
    return className;
  }
  
  /**
   * @param className  The class name
   */
  public void setClassName(String className) {
    this.className = className;
  }
  
  /**
   * @return  The return
   */
  public Long getClassSerialVersionUid() {
    return classSerialVersionUid;
  }

  /**
   * @param classSerialVersionUid  The class serial version Uid
   */
  public void setClassSerialVersionUid(Long classSerialVersionUid) {
    this.classSerialVersionUid = classSerialVersionUid;
  }
  
  /**
   * @return  The return
   */
  public boolean isSerializable() {
    return serializable;
  }
  
/**
 * @param serializable  The serializable
 */
  public void setSerializable(boolean serializable) {
    this.serializable = serializable;
  }
  
  /**
   * @return  The return
   */
  public String getSuperClassName() {
    return superClassName;
  }
  
  /**
   * @param superClassName  The super class name
   */
  public void setSuperClassName(String superClassName) {
   this.superClassName = superClassName;
  }
  
  /**
   * @return  The return
   */
  public Set<String> getInterfaces() {
    return interfaces;
  }
  
  /**
   * @param interfaces  The interfaces
   */
  public void setInterfaces(Set<String> interfaces) {
    this.interfaces = interfaces;
  }

}
