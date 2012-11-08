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
 * @(#)EncodingProperties.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * The instance of the class is used to hold an immutable set of encoder
 * properties.
 *
 * @author Jun Xu
 */
public class EncoderProperties implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * Pre-decode character coding
     */
    public static final String PREDECODE_CHAR_CODING =
        "predecoding_char_coding";
    /**
     * Post-encode character coding
     */
    public static final String POSTENCODE_CHAR_CODING =
        "postencoding_char_coding";
    public static final Map<Object, Object> mKeyAliases =
        new HashMap<Object, Object>();

    static {
        mKeyAliases.put("pdcc", PREDECODE_CHAR_CODING);
        mKeyAliases.put("pecc", POSTENCODE_CHAR_CODING);
    }

    private final boolean mImmutable;

    private Map<Object, Object> mProperties = new HashMap<Object, Object>();

    private EncoderProperties(boolean immutable) {
        mImmutable = immutable;
    }

    public EncoderProperties() {
        this(false);
    }

    /**
     * Sets the pre-decoding character coding. The character coding can either
     * be a Java character set name or some special coding methods such as
     * "base64binary" and "hexbinary".
     *
     * @param coding the pre-decoding character coding
     */
    public void setPreDecodeCharCoding(String coding) {
        mProperties.put(PREDECODE_CHAR_CODING, coding);
    }

    /**
     * Gets the pre-decoding charatcer coding.
     *
     * @return the pre-decoding charatcer coding
     */
    public String getPreDecodeCharCoding() {
        return (String) mProperties.get(PREDECODE_CHAR_CODING);
    }

    /**
     * Sets the post-encoding character coding.
     *
     * @param coding the post-encoding character coding
     */
    public void setPostEncodeCharCoding(String coding) {
        mProperties.put(POSTENCODE_CHAR_CODING, coding);
    }

    /**
     * Gets the post-encoding character coding.
     *
     * @return the post-encoding character coding.
     */
    public String getPostEncodeCharCoding() {
        return (String) mProperties.get(POSTENCODE_CHAR_CODING);
    }

    /**********************************
     *  Following are Generic methods *
     **********************************
     */

    public void addAll(EncoderProperties props) {
        assertAccess("addAll");
        mProperties.putAll(props.mProperties);
    }

    public void  setProperty(Object propertyKey)
            throws UnsupportedOperationException {
        setProperty(propertyKey, null);
    }

    public boolean hasProperty(Object propertyKey) {
        return mProperties.containsKey(propertyKey);
    }

    public void  setProperty(Object propertyKey, Object value)
            throws UnsupportedOperationException {
        assertAccess("setProperty()");
        if (mKeyAliases.containsKey(propertyKey)) {
            mProperties.put(mKeyAliases.get(propertyKey), value);
        } else {
            mProperties.put(propertyKey, value);
        }
    }

    public Object getProperty(Object propertyKey) {
        if (mKeyAliases.containsKey(propertyKey)) {
            return mProperties.get(mKeyAliases.get(propertyKey));
        }
        return mProperties.get(propertyKey);
    }

    @SuppressWarnings("unchecked")
    public <T> T getProperty(Object propertyKey, Class<T> clazz) {
        return (T) mProperties.get(propertyKey);
    }

    public void removeProperty(Object propertyKey)
            throws UnsupportedOperationException {
        assertAccess("removeProperty()");
        mProperties.remove(propertyKey);
    }

    public boolean isEmpty() {
        return mProperties.isEmpty();
    }

    public Set<Map.Entry<Object, Object>> getProperties() {
        if (mImmutable) {
            return Collections.unmodifiableSet(mProperties.entrySet());
        }
        return mProperties.entrySet();
    }

    public boolean immutable() {
        return mImmutable;
    }

    public EncoderProperties cloneMutable() {
        EncoderProperties props = new EncoderProperties(false);
        props.mProperties.putAll(mProperties);
        return props;
    }

    public EncoderProperties cloneImmutable() {
        EncoderProperties props = new EncoderProperties(true);
        props.mProperties.putAll(mProperties);
        return props;
    }

    private void assertAccess(String operation) {
        if (mImmutable) {
            throw new UnsupportedOperationException(
                    new StringBuilder("The operation ")
                    .append(operation).append(" is not supported because ")
                    .append("the property set is immutable.").toString());
        }
    }

    @Override
    public String toString() {
        StringBuffer buf = new StringBuffer("EncoderProperties@");
        buf.append(Integer.toHexString(hashCode()));
        buf.append(" immutable=").append(mImmutable);
        buf.append(" properties=").append(mProperties);
        return buf.toString();
    }
}
