/*
 *  Copyright (c) 2005, 2006 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 */


package it.imolinfo.jbi4cics.typemapping.cobol;

import it.imolinfo.jbi4cics.Logger;
import it.imolinfo.jbi4cics.LoggerFactory;
import it.imolinfo.jbi4cics.exception.FormatException;
import it.imolinfo.jbi4cics.messageformat.FieldDescriptor;
import it.imolinfo.jbi4cics.messageformat.commarea.CommareaBeanMappingDescriptor;
import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.apache.commons.lang.builder.ReflectionToStringBuilder;

/**
 * @author raffaele
 * @author <a href="mailto:mcimatti@imolinfo.it">Marco Cimatti</a>
 */
public class CobolTypeDescriptor implements FieldDescriptor {


    // Justification


    public final static int STRING_JUSTIFICATION_LEFT = 0;

    public final static int STRING_JUSTIFICATION_RIGHT = 1;

    public final static int STRING_JUSTIFICATION_CENTER = 2;


    // Zoned Sign Format


    public final static int SIGN_FORMAT_LEADING = 0;

    public final static int SIGN_FORMAT_TRAILING = 1;

    public final static int SIGN_FORMAT_LEADING_SEPARATE = 2;


    /**
     * The logger for this class and its instances.
     */
    private static final Logger LOG
            = LoggerFactory.getLogger(CobolTypeDescriptor.class);

    /**
     * The field name as it appears in the commarea.
     */
    private String name;

    /**
     * The Cobol type described by this instance.
     */
    private CobolType type;


    // dati aggiuntivi stringa


    // E' il default dell'host
    private int justification = STRING_JUSTIFICATION_LEFT;

    private String padCharacter = " ";

    private int stringLength;

    private String codePage;


    // dati aggiuntivi numeric


    private boolean signed;


    // dati aggiuntivi integer


    private boolean bigEndian;


    // dati agiuntivi decimal


    private int integerPartLength;

    private int decimalPartLength;


    // dati aggiuntivi zoned


    private int zonedSignFormat = SIGN_FORMAT_LEADING;


    // dati aggiuntivi per il nesting


    private int level;

    private CommareaBeanMappingDescriptor nestedCommarea;


    // dati aggiuntivi per occurs


    private int occursSize;


    /**
     * Costruttore di default: sarebbe da usare solo nei test.
     */
    public CobolTypeDescriptor() {
    }

    @Override
    public String toString() {                                  // Overridden
        return ReflectionToStringBuilder.toString(this);
    }

    @Override
    public boolean equals(Object obj) {                         // Overridden
        CobolTypeDescriptor that;

        if (obj == this) {
            return true;
        }
        if (!(obj instanceof CobolTypeDescriptor)) {
            return false;
        }
        that = (CobolTypeDescriptor) obj;
        return new EqualsBuilder().append(bigEndian, that.bigEndian)
                .append(codePage, that.codePage)
                .append(decimalPartLength, that.decimalPartLength)
                .append(integerPartLength, that.integerPartLength)
                .append(justification, that.justification)
                .append(level, that.level)
                .append(name, that.name)
                .append(nestedCommarea, that.nestedCommarea)
                .append(occursSize, that.occursSize)
                .append(padCharacter, that.padCharacter)
                .append(signed, that.signed)
                .append(stringLength, that.stringLength)
                .append(type, that.type)
                .append(zonedSignFormat, that.zonedSignFormat).isEquals();
    }

    @Override
    public int hashCode() {                                     // Overridden
        return new HashCodeBuilder().append(bigEndian)
                .append(codePage)
                .append(decimalPartLength)
                .append(integerPartLength)
                .append(justification)
                .append(level)
                .append(name)
                .append(nestedCommarea)
                .append(occursSize)
                .append(padCharacter)
                .append(signed)
                .append(stringLength)
                .append(type)
                .append(zonedSignFormat).toHashCode();
    }

    /**
     * Returns the level.
     *
     * @return the level.
     */
    public int getLevel() {
        return level;
    }

    /**
     * Sets the level.
     *
     * @param  level  the new level to set.
     */
    public void setLevel(int level) {
        this.level = level;
    }

    /**
     * @return Returns the signed.
     */
    public boolean isSigned() {
        return signed;
    }

    /**
     * @param signed
     *            The signed to set.
     */
    public void setSigned(boolean signed) {
        this.signed = signed;
    }

    /**
     * @return Returns the justification.
     */
    public int getJustification() {
        return justification;
    }

    /**
     * @param justification
     *            The justification to set.
     */
    public void setJustification(int justification) {
        this.justification = justification;
    }

    /**
     * @return Returns the padCharacter.
     */
    public String getPadCharacter() {
        return padCharacter;
    }

    /**
     * @param padCharacter
     *            The padCharacter to set.
     */
    public void setPadCharacter(String padCharacter) {
        this.padCharacter = padCharacter;
    }

    /**
     * @return Returns the decimalPartLength.
     */
    public int getDecimalPartLength() {
        return decimalPartLength;
    }

    /**
     * @param decimalPartLength
     *            The decimalPartLength to set.
     */
    public void setDecimalPartLength(int decimalPartLength) {
        this.decimalPartLength = decimalPartLength;
    }

    /**
     * @return Returns the integerPartLength.
     */
    public int getIntegerPartLength() {
        return integerPartLength;
    }

    /**
     * @param integerPartLength
     *            The integerPartLength to set.
     */
    public void setIntegerPartLength(int integerPartLength) {
        this.integerPartLength = integerPartLength;
    }

    /**
     * @return Returns the stringLength.
     */
    public int getStringLength() {
        return stringLength;
    }

    /**
     * @param stringLength
     *            The stringLength to set.
     */
    public void setStringLength(int stringLength) {
        this.stringLength = stringLength;
    }

    /**
     * @return Returns the type.
     */
    public CobolType getType() {
        return type;
    }

    /**
     * @param type
     *            The type to set.
     */
    public void setType(CobolType type) {
        this.type = type;
    }

    /**
     * @return Returns the virtualDecimalPoint.
     */
    public int getVirtualDecimalPoint() {
        return getDecimalPartLength();
    }

    /**
     * Returns the buffered length.
     *
     * @return  the buffered length.
     */
    public int getBufferedLength() throws FormatException {
        CobolType ct = getType();

        if (ct == null) {
            LOG.error("CIC002110_Unknown_cobol_type", ct);
            throw new FormatException("CIC002110_Unknown_cobol_type",
                                      new Object[] { ct });
        }
        return ct.getBufferedLength(this);
    }

    /**
     * @return Returns the bigEndian.
     */
    public boolean isBigEndian() {
        return bigEndian;
    }

    /**
     * @param bigEndian
     *            The bigEndian to set.
     */
    public void setBigEndian(boolean bigEndian) {
        this.bigEndian = bigEndian;
    }

    /**
     * Returns the code page.
     *
     * @return  the code page.
     */
    public String getCodePage() {
        return codePage;
    }

    /**
     * Sets the code page.
     *
     * @param  codePage  the code page to set.
     */
    public void setCodePage(String codePage) {
        CobolType ct = getType();

        this.codePage = codePage;
        if ((ct == CobolType.NESTED_COMMAREA) || (ct == CobolType.OCCURS)) {
            nestedCommarea.setCodePage(codePage);
        }
    }

    /**
     * @return Returns the zonedSignFormat.
     */
    public int getZonedSignFormat() {
        return zonedSignFormat;
    }

    /**
     * @param zonedSignFormat
     *            The zonedSignFormat to set.
     */
    public void setZonedSignFormat(int zonedSignFormat) {
        this.zonedSignFormat = zonedSignFormat;
    }

    /**
     * @return Returns the name.
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     *            The name to set.
     */
    public void setName(String name) {
        this.name = name;
    }

    public Class getPreferredJavaType() throws FormatException {
        CobolType ct = getType();

        if (ct == null) {
            LOG.error("CIC002111_Unexpected_cobol_type", ct);
            throw new FormatException("CIC002111_Unexpected_cobol_type",
                                      new Object[] { ct });
        }
        return ct.getPreferredJavaType(this);
    }

    public CommareaBeanMappingDescriptor getNestedCommarea() {
        return nestedCommarea;
    }

    public void setNestedCommarea(CommareaBeanMappingDescriptor nestedCommarea) {
        this.nestedCommarea = nestedCommarea;
    }

    public int getOccursSize() {
        return occursSize;
    }

    public void setOccursSize(int occursSize) {
        this.occursSize = occursSize;
    }
}
