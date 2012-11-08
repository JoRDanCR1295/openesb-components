/*
 * BEGIN_HEADER - DO NOT EDIT
 *
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)EncoderType.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder;

/**
 * The instance of the EncoderType class is used to identify a certain type
 * of encoder. An encoder type might be basic or might be derived, which is
 * an basic encoder type plus a few custom properties. Basic encoder type can
 * be used to identify an encoder provider because encoder identification,
 * instance of basic encoder type and instance of encoder provider have
 * one-to-one relationship.
 *
 * @author Jun Yang, Jun Xu
 */
public final class EncoderType implements java.io.Serializable {

    private static final long serialVersionUID = 1L;

    private final EncoderType mBasicType;
    private final String mIdentification;
    private final DataNature mDataNature;
    private final EncoderProperties mProperties;

    /**
     * Construct a new EncoderType based on identification and DataNature.
     */
    EncoderType(String identification, DataNature dataNature) {
        if (identification == null) {
            throw new IllegalArgumentException("null identification");
        }

        if (dataNature == null) {
            throw new IllegalArgumentException("null data nature");
        }

        mBasicType = null;
        mIdentification = identification;
        mDataNature = dataNature;
        mProperties = null;
    }

    /**
     * Construct a new EncoderType based on EncoderType and EncoderProperties.
     */
    EncoderType(EncoderType basicType, EncoderProperties properties) {
        if (basicType == null) {
            throw new IllegalArgumentException("null basic encoder type");
        }
        mBasicType = basicType;
        mIdentification = basicType.getIdentification();
        mDataNature = basicType.getDataNature();
        mProperties = properties;
    }

    /**
     * Returns whether this EncoderType is a basic type.
     * @return true if this EncoderType is a basic type, or false
     * otherwise.
     */
    public boolean isBasic() {
        return mBasicType == null;
    }

    /**
     * Gets the identification of the encoder type.
     *
     * @return the identification of the encoder type
     */
    public String getIdentification() {
        return mIdentification;
    }

    /**
     * Queries the data nature of the encoder type.
     *
     * @return the data nature of the encoder type.
     */
    public DataNature getDataNature() {
        return mDataNature;
    }

    /**
     * Gets the basic encoder type of this encoder type. The instance of
     * the basic encoder type is one-to-one to the encoder type identification,
     * so it can be used to look up encoder provider in EncoderFactory.
     *
     * @return the instance of the basic encoder type
     */
    public EncoderType getBasicType() {
        if (mBasicType == null) {
            return this;
        }
        return mBasicType;
    }

    /**
     * Gets the properties of a non-basic encoder type.
     *
     * @return the properties of a non-basic encoder type
     */
    public EncoderProperties getEncoderProperties() {
        return mProperties;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (isBasic()) {
            sb.append("basic");
        } else {
            sb.append("non-basic,basicType=[").append(mBasicType).append("]");
        }
        sb.append(" Id=").append(mIdentification);
        sb.append(" dataNature=").append(mDataNature);
        sb.append(" encoderProperties=").append(mProperties);
        return sb.toString();
    }
}
