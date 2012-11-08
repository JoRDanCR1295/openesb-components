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
 * @(#)Encoder.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;

import javax.xml.transform.Source;


/**
 * This interface defines encoding and decoding services for message exchanges,
 * where encoding is a process meant for converting a message from XML format
 * into a specific custom format (such as IDOC, HL7, SWIFT or any kind of
 * proprietary custom formats) and decoding is the reverse operation of
 * encoding.
 *
 * <p>An encoder typically carries two aspects of information.  One aspect is
 * the encoding style, which implies a set of default encoding rules for that
 * encoding style.  The other aspect of the information is the metadata, which
 * describes the message structure and message type specific encoding rules.
 * With that said, basically an instance of this interface is bound
 * to a specific encoding style, a specific message type and a set of encoding
 * rules that are specific to that message type.  Users of the instance must
 * be careful to only feed input that matches the encoder definition.
 *
 * @author Jun Yang, Jun Xu
 */

public interface Encoder {

    /**
     * Decodes a string message encoded in custom format into an XML encoded
     * message.
     *
     * @param input the string message encoded in custom format
     * @return the message encoded in XML
     * @throws EncoderException -
     */
    Source decodeFromString(String input) throws EncoderException;

    /**
     * Encodes a message encoded in XML into a string message encoded in
     * custom format.
     *
     * @param src the message encoded in XML
     * @return the string message encoded in custom format
     * @throws EncoderException
     */
    String encodeToString(Source src) throws EncoderException;

    /**
     * Decodes a byte array message encoded in custom format into an XML
     * encoded message.
     *
     * @param input the byte array message encoded in custom format
     * @return the message encoded in XML
     * @throws EncoderException
     */
    Source decodeFromBytes(byte[] input) throws EncoderException;

    /**
     * Encodes a message encoded in XML into a byte array message encoded in
     * custom format.
     *
     * @param src the message encoded in XML
     * @return the byte array message encoded in custom format
     * @throws EncoderException
     */
    byte[] encodeToBytes(Source src) throws EncoderException;

    /**
     * Gives the encoder a chance to do clean-ups proactively.  If the encoder
     * uses generated code, it can use this chance to free up class loaders,
     * remove temporary jar files and folders.  It is highly recommended to
     * call this method when an encoder is no longer needed.  After this method
     * is called, calling other methods on the encoder instance will have
     * unpredictable result.  But calling this method multiple times after it
     * has been called shall be safe and make no effect.
     *
     * @return <code>true</code> if the disposal process is successful,
     *         otherwise <code>false</code>
     */
    boolean dispose();

    /**
     * Queries the encoder type of this encoder.
     *
     * @return the type of the encoder
     */
    EncoderType getType();

    /**
     * Gets the encoder properties associated with the encoder instance.
     *
     * @return the immutable encoder properties
     */
    EncoderProperties getProperties();

    /**
     * Decodes a Reader encoded in custom format into an XML encoded
     * message.
     *
     * @param input the string message encoded in custom format
     * @return the message encoded in XML
     * @throws EncoderException
     */
    Source decodeFromReader(Reader input) throws EncoderException;

    /**
     * Encodes a message encoded in XML into a string message encoded in
     * custom format.
     *
     * @param in the message encoded in XML
     * @param out the message in custom format
     * @throws EncoderException
     */
    void encodeToWriter(Source in, Writer out) throws EncoderException;

    /**
     * Decodes an InputStream message encoded in custom format into an XML
     * encoded message.
     *
     * @param input the byte array message encoded in custom format
     * @return the message encoded in XML
     * @throws EncoderException
     */
    Source decodeFromStream(InputStream input) throws EncoderException;

    /**
     * Encodes a message encoded in XML into an OutputStream encoded in
     * custom format.
     *
     * @param in the message encoded in XML
     * @param out the message in custom format
     * @throws EncoderException
     */
    void encodeToStream(Source in, OutputStream out) throws EncoderException;

    /**
     * Decodes a string message encoded in custom format into an XML encoded
     * message. Support passing in additional properties.
     *
     * @param input the string message encoded in custom format
     * @param properties the additional encoder properties
     * @return the message encoded in XML
     * @throws EncoderException
     */
    Source decodeFromString(String input, EncoderProperties properties)
        throws EncoderException;

    /**
     * Encodes a message encoded in XML into a string message encoded in
     * custom format. Support passing in additional properties.
     *
     * @param src the message encoded in XML
     * @param properties the additional encoder properties
     * @return the string message encoded in custom format
     * @throws EncoderException
     */
    String encodeToString(Source src, EncoderProperties properties)
        throws EncoderException;

    /**
     * Decodes a byte array message encoded in custom format into an XML
     * encoded message. Support passing in additional properties.
     *
     * @param input the byte array message encoded in custom format
     * @param properties the additional encoder properties
     * @return the message encoded in XML
     * @throws EncoderException
     */
    Source decodeFromBytes(byte[] input, EncoderProperties properties)
        throws EncoderException;

    /**
     * Encodes a message encoded in XML into a byte array message encoded in
     * custom format. Support passing in additional properties.
     *
     * @param src the message encoded in XML
     * @param properties the addtional encoder properties
     * @return the byte array message encoded in custom format
     * @throws EncoderException
     */
    byte[] encodeToBytes(Source src, EncoderProperties properties)
        throws EncoderException;

    /**
     * Decodes a Reader encoded in custom format into an XML encoded
     * message. Support passing in additional properties.
     *
     * @param input the string message encoded in custom format
     * @param properties the additional encoder properties
     * @return the message encoded in XML
     * @throws EncoderException
     */
    Source decodeFromReader(Reader input, EncoderProperties properties)
        throws EncoderException;

    /**
     * Encodes a message encoded in XML into a string message encoded in
     * custom format. Support passing in additional properties.
     *
     * @param in the message encoded in XML
     * @param out the message in custom format
     * @param properties the additional encoder properties
     * @throws EncoderException
     */
    void encodeToWriter(Source in, Writer out, EncoderProperties properties)
        throws EncoderException;

    /**
     * Decodes an InputStream message encoded in custom format into an XML
     * encoded message. Support passing in additional properties.
     *
     * @param input the byte array message encoded in custom format
     * @param properties the additional encoder properties
     * @return the message encoded in XML
     * @throws EncoderException
     */
    Source decodeFromStream(InputStream input, EncoderProperties properties)
        throws EncoderException;

    /**
     * Encodes a message encoded in XML into an OutputStream encoded in
     * custom format. Support passing in additional properties.
     *
     * @param in the message encoded in XML
     * @param out the message in custom format
     * @param properties the additional encoder properties
     * @throws EncoderException
     */
    void encodeToStream(Source in, OutputStream out,
            EncoderProperties properties) throws EncoderException;
}
