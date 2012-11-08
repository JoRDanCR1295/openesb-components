/*******************************************************************************
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 ******************************************************************************/


package it.imolinfo.jbi4cics.commareaparser;

import it.imolinfo.jbi4cics.exception.ParseException;
import it.imolinfo.jbi4cics.typemapping.cobol.CobolType;
import it.imolinfo.jbi4cics.typemapping.cobol.CobolTypeDescriptor;

public class OccursDefinition {

  private int minSize;
  private int maxSize;
  private int size;
  private String variableName;
  private boolean dynamic;

  public OccursDefinition() {
  }

  /**
   * @return the dynamic
   */
  public boolean isDynamic() {
    return dynamic;
  }
  /**
   * @param dynamic the dynamic to set
   */
  public void setDynamic(boolean dynamic) {
    this.dynamic = dynamic;
  }
  /**
   * @return the maxSize
   */
  public int getMaxSize() {
    return maxSize;
  }
  /**
   * @param maxSize the maxSize to set
   */
  public void setMaxSize(int maxSize) {
    this.maxSize = maxSize;
  }
  /**
   * @return the minSize
   */
  public int getMinSize() {
    return minSize;
  }
  /**
   * @param minSize the minSize to set
   */
  public void setMinSize(int minSize) {
    this.minSize = minSize;
  }
  /**
   * @return the size
   */
  public int getSize() {
    return size;
  }
  /**
   * @param size the size to set
   */
  public void setSize(int size) {
    this.size = size;
  }
  /**
   * @return the variableName
   */
  public String getVariableName() {
    return variableName;
  }
  /**
   * @param variableName the variableName to set
   */
  public void setVariableName(String variableName) {
    this.variableName = variableName;
  }

  public void populate(CobolTypeDescriptor cobolTypeDescriptor)
          throws ParseException{
    cobolTypeDescriptor.setType(CobolType.OCCURS);
    if (isDynamic()){
      throw new ParseException("CIC000001_Occurs_not_supported");
    }
    else {
      cobolTypeDescriptor.setOccursSize(size);
    }
  }
}
