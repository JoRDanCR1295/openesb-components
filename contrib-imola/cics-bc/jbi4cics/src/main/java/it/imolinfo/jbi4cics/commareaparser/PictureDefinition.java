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

/**
 * this class is required by the parser and is used to store information about
 * the picure part of cobol tipe definition.
 *
 * @author raffaele
 */
public class PictureDefinition {

  /**
   * the constant NUMERIC means that cobol type is numeric.
   */
  public static final int NUMERIC = 0;

  /**
   * the constant STRING means that cobol type is a string.
   */
  public static final int STRING = 1;

  /**
   * the type of this PictureDefinition.
   * Possible type are @see NUMERIC and @see STRING
   */
  private int type;

  /**
   * the string length in case of string.
   */
  private int stringLength;

  /**
   * the integer part length in case of numeric.
   */
  private int integerLength;

  /**
   * the decimal part length in case of numeric decimal.
   */
  private int decimalLength;

  private boolean signed;

  private boolean signPlus;

  public PictureDefinition() {
  }

  /**
   * @return Returns the decimalLength.
   */
  public int getDecimalLength() {
    return decimalLength;
  }

  /**
   * @param decimalLength
   *          The decimalLength to set.
   */
  public void setDecimalLength(int decimalLength) {
    this.decimalLength = decimalLength;
  }

  /**
   * @return Returns the integerLength.
   */
  public int getIntegerLength() {
    return integerLength;
  }

  /**
   * @param integerLength
   *          The integerLength to set.
   */
  public void setIntegerLength(int integerLength) {
    this.integerLength = integerLength;
  }

  /**
   * @return Returns the stringLength.
   */
  public int getStringLength() {
    return stringLength;
  }

  /**
   * @param stringLength
   *          The stringLength to set.
   */
  public void setStringLength(int stringLength) {
    this.stringLength = stringLength;
  }

  /**
   * @return Returns the type.
   */
  public int getType() {
    return type;
  }

  /**
   * @param type
   *          The type to set.
   */
  public final void setType(final int type) {
    this.type = type;
  }

  public void populate(CobolTypeDescriptor cobolTypeDescriptor)
          throws ParseException {
    switch (type) {
      case NUMERIC:
        cobolTypeDescriptor.setType(CobolType.ZONED);
        cobolTypeDescriptor.setDecimalPartLength(decimalLength);
        cobolTypeDescriptor.setIntegerPartLength(integerLength);
        cobolTypeDescriptor.setSigned(signed);
        if (signPlus) {
          cobolTypeDescriptor.setZonedSignFormat(
                  CobolTypeDescriptor.SIGN_FORMAT_LEADING_SEPARATE);
        }
        break;

      case STRING:
        cobolTypeDescriptor.setType(CobolType.STRING);
        cobolTypeDescriptor.setStringLength(stringLength);
        cobolTypeDescriptor.setPadCharacter(" ");
        cobolTypeDescriptor.setJustification(
                CobolTypeDescriptor.STRING_JUSTIFICATION_LEFT);
        break;

      default:
        throw new ParseException("CIC000002_Unrecognized_type",
                new Object[] {type});
    }
  }

  /**
   * @return Returns the signed.
   */
  public boolean isSigned() {
    return signed;
  }

  /**
   * @param signed
   *          The signed to set.
   */
  public void setSigned(boolean signed) {
    this.signed = signed;
  }

  /**
   * @return Returns the signPlus.
   */
  public boolean isSignPlus() {
    return signPlus;
  }

  /**
   * @param signPlus
   *          The signPlus to set.
   */
  public void setSignPlus(boolean signPlus) {
    this.signPlus = signPlus;
  }
}
