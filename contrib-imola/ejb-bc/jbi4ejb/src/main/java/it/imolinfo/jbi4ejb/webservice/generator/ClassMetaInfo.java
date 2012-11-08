/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4ejb.webservice.generator;

import java.util.HashSet;
import java.util.Set;

/**
 * The meta information of a class.
 */
public class ClassMetaInfo {

  /** The class name. */
  private String className = null;
  
  /** The super class name. */
  private String superClassName = null;

  /** The interfaces. */
  private Set<String> interfaces = new HashSet<String>();

  /** The serializable. */
  private boolean serializable = false;
  
  /** The class serial version uid. */
  private Long classSerialVersionUid = null;

  /**
    * Constructor.
    */
  public ClassMetaInfo() {
    // NOP
  }

  // getter and setter

  /**
     * Gets the class name.
     * 
     * @return the class name
     */
  public String getClassName() {
    return className;
  }
  
  /**
     * Sets the class name.
     * 
     * @param className
     *            the new class name
     */
  public void setClassName(String className) {
    this.className = className;
  }
  
  /**
     * Gets the class serial version uid.
     * 
     * @return the class serial version uid
     */
  public Long getClassSerialVersionUid() {
    return classSerialVersionUid;
  }
  
  /**
     * Sets the class serial version uid.
     * 
     * @param classSerialVersionUid
     *            the new class serial version uid
     */
  public void setClassSerialVersionUid(Long classSerialVersionUid) {
    this.classSerialVersionUid = classSerialVersionUid;
  }
  
  /**
     * Checks if is serializable.
     * 
     * @return true, if is serializable
     */
  public boolean isSerializable() {
    return serializable;
  }
  
  /**
     * Sets the serializable.
     * 
     * @param serializable
     *            the new serializable
     */
  public void setSerializable(boolean serializable) {
    this.serializable = serializable;
  }
  
  /**
     * Gets the super class name.
     * 
     * @return the super class name
     */
  public String getSuperClassName() {
    return superClassName;
  }
  
  /**
     * Sets the super class name.
     * 
     * @param superClassName
     *            the new super class name
     */
  public void setSuperClassName(String superClassName) {
   this.superClassName = superClassName;
  }
  
  /**
     * Gets the interfaces.
     * 
     * @return the interfaces
     */
  public Set<String> getInterfaces() {
    return interfaces;
  }
  
  /**
     * Sets the interfaces.
     * 
     * @param interfaces
     *            the new interfaces
     */
  public void setInterfaces(Set<String> interfaces) {
    this.interfaces = interfaces;
  }

}
