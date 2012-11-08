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
 * @(#)MessageImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco.runtime.messages;

import java.text.MessageFormat;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

class MessageImpl
        implements Message
{
    /**
     * Create a Message instance.
     *
     * @param id Message identity, must match a key in
     *           com.sun.encoder.coco.runtime.messages.Message.properties.
     *
     * @throws NullPointerException     if <code>id</code> is <code>null</code>
     * @throws MissingResourceException if <code>id</code> cannot be resolved
     */ 
    public MessageImpl(String id)
    {
        mText = cMSGTEXTS.getString(id);
        mId = id;
        mToString = mId + ": " + mText;
    }

    /**
     * Message identification.
     */
    public Object id()
    {
        return mId;
    }

    /**
     * Textual content of the Message.
     */
    public String text()
    {
        return mText;
    }

    /**
     * Treat the text of the Message (as produced by {@link #toString}) as a
     * {@link java.text.MessageFormat} pattern, formatting the specified
     * arguments against it by delegating the task to
     * {@link java.text.MessageFormat#format(String, Object[])}.
     *
     * @param args Arguments to apply to the Message's pattern
     *
     * @return An instantiation of the pattern as returned by {@link
     *         java.text.MessageFormat#format}
     */
    public String formatText(Object[] args)
    {
        synchronized(this) {
            String text;
            
            if (mFormatter == null) {
                mFormatter = new MessageFormat(mToString);
            }
            
            text = mFormatter.format(args, mFormatBuffer, null).toString();
            mFormatBuffer.delete(0, mFormatBuffer.length());
            return text;
        }
    }

    /**
     * Returns a string representation of the object. In general, the
     * <code>toString</code> method returns a string that "textually represents"
     * this object. The result should be a concise but informative
     * representation that is easy for a person to read. It is recommended that
     * all subclasses override this method.
     * <p/>
     * The <code>toString</code> method for class <code>Object</code> returns a
     * string consisting of the name of the class of which the object is an
     * instance, the at-sign character `<code>@</code>', and the unsigned
     * hexadecimal representation of the hash code of the object. In other
     * words, this method returns a string equal to the value of: <blockquote>
     * <pre>
     * getClass().getName() + '@' + Integer.toHexString(hashCode())
     * </pre></blockquote>
     *
     * @return a string representation of the object.
     */
    @Override
    public String toString()
    {
        return mToString;
    }

    private static final ResourceBundle cMSGTEXTS =
            ResourceBundle.getBundle(
                    "com/sun/encoder/coco/runtime/messages/Messages");
    
    private MessageFormat mFormatter;
    private final StringBuffer mFormatBuffer = new StringBuffer();
    private final String mText;
    private final String mId;
    private String mToString;
}

// FINIS $RCSfile: MessageImpl.java,v $
