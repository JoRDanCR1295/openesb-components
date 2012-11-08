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
 * @(#)SMTPMailTo.java 
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.binding.email.protocol.send.smtp;

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.sun.jbi.binding.email.I18n;

/**
 * The SMTPMailTo class provides an interpretation of the mailto URI syntax as per
 * RFC 2368.  The mailto URI designates an Internet mailing address of
 * individuals.  It allows for multiple senders and the setting of mail header
 * fields.
 * <p>
 * Encoding is a very tricky part of the URI syntax.  The SMTPMailTo class only
 * accepts properly encoded strings.  The unmarshal() method will fail if
 * given an improper encoding.  The marshal() method only returns encoded
 * strings.  Once a mailto URI is unmarshalled, calls to methods getMailBox(),
 * addMailbox(), addHeader(), getHeader(), and getHeaders() will refer to the 
 * unencoded strings.  Using encoded strings with these methods will result
 * in unpredicatable behavior if the SMTPMailTo class is unmarshalled.  The
 * reasoning for this decision is that most
 * users of the class only have to deal with encodings at the unmarshal and
 * marshal level.  Most times they don't want to have to deal with it when
 * working with the components of the mailto URI
 * <p>
 * The encoding scheme is defined by RFC 2368 and RFC 1738.
 * The standard URL encoding mechanisms ("%" followed by a two-digit hex number) 
 * must be used in certain cases.
 * <ul>
 * <li>All unsafe characters as defined by RFC 1738 must be encoded.  For the
 * mailto URI this includes common characters such as the space, the quote, the
 * less-than-sign, the greater-than-sign, carriage-return (CR), line-feed (LF).</li>
 * <p>
 * <blockquote>
 *       mailto:foo@domain.com?subject=a header     // WRONG!
 *       mailto:foo@domain.com?subject=a%20header   // RIGHT
 *       mailto:foo@domain.com?subject=a>b&body=firstLine
 *       secondLine                                     // WRONG!
 *       mailto:foo@domain.com?subject=a%3Eb&body=firstLine%0D%0AsecondLine  // RIGHT
 * </blockquote>
 * <li>All alphanumeric characters do NOT have to be encoded.</li>
 * <li>All special characters $-_.+!*'(), do NOT have to be encoded.</li>
 * <li>The question mark (?), the equals sign (=), and the ampersand sign (&) are
 * reserved characters in the mailto URI scheme.  That means if they are used in
 * the content of mailbox or header, they must be encoded.
 * <blockquote>
 *       mailto:foo@domain.com?subject=a&b     // WRONG!
 *       mailto:foo@domain.com?subject=a%26b   // RIGHT
 *       mailto:foo@domain.com?subject=a=b&body=ok?no      // WRONG!
 *       mailto:foo@domain.com?subject=a%3Db&body=ok%3Fno  // RIGHT
 * </blockquote>
 * </li>
 * <li>All other URL reserved characters that are not being used for the mailto
 * URI scheme do NOT have to be encoded.  This includes semi-colon (;),
 * slash (/), colon (:), and the at-sign ("@").
 * </ul>
 *
 * @author Harry Liu (Harry.Liu@sun.com)
 *
 */
public class SMTPMailTo {

    private static final Logger logger = Logger.getLogger(SMTPMailTo.class.getName());
    public static final String MAILTO_SCHEME = "mailto";
    public static final String MAILTO_SCHEME_WITH_DELIMITER = MAILTO_SCHEME + ":";
    private Map<String, String> mHeaders;
    private Collection<SMTPMailbox> mMailboxes;
    private Collection<SMTPMailbox> mCCMailboxes;
    private Collection<SMTPMailbox> mBCCMailboxes;
    private SMTPMailbox mFromMailbox;
    private String subject;
    private String body;
    private String newsgroups;

    public SMTPMailTo() {
        mHeaders = new HashMap<String, String>();
        mMailboxes = new ArrayList<SMTPMailbox>();
        mCCMailboxes = new ArrayList<SMTPMailbox>();
        mBCCMailboxes = new ArrayList<SMTPMailbox>();
        mFromMailbox = null;
        subject = "";
        body = "";
        newsgroups = "";
    }

    public SMTPMailTo(final String mailToURI) throws URISyntaxException {
        this();
        unmarshal(new URI(mailToURI));
    }

    public String getProtocol() {
        return SMTPMailTo.MAILTO_SCHEME;
    }

    public Collection<SMTPMailbox> getMailbox() {
        return mMailboxes;
    }

    public Collection<SMTPMailbox> getCCMailbox() {
        return mCCMailboxes;
    }

    public Collection<SMTPMailbox> getBCCMailbox() {
        return mBCCMailboxes;
    }

    public SMTPMailbox getFromMailbox() {
        return mFromMailbox;
    }

    public String getSubject() {
        return subject;
    }

    public String getBody() {
        return body;
    }

    public String getNewsgroups() {
        return newsgroups;
    }

    public void addMailbox(final SMTPMailbox mailbox) {
        mMailboxes.add(mailbox);
    }

    public void addHeader(final String name, final String value) {
        mHeaders.put(name, value);
    }

    public String getHeader(final String name) {
        return mHeaders.get(name);
    }

    public Map<String, String> getHeaders() {
        return mHeaders;
    }

    public String marshal() throws URISyntaxException {
        final StringBuffer result = new StringBuffer();
        result.append(SMTPMailTo.MAILTO_SCHEME_WITH_DELIMITER);
        final Iterator<SMTPMailbox> it = mMailboxes.iterator();
        while (it.hasNext()) {
            final SMTPMailbox mailBox = it.next();
            try {
                String encodedMailbox = URLEncoder.encode(mailBox.marshal(),
                        "UTF-8");
                encodedMailbox = encodedMailbox.replace("+", "%20");
                encodedMailbox = encodedMailbox.replace("%40", "@");
                result.append(encodedMailbox);
            } catch (final Exception ex) {
                final URISyntaxException mex =
                        new URISyntaxException(mailBox.getAddressSpec(), "");
                mex.initCause(ex);
                throw mex;
            }
            if (it.hasNext()) {
                result.append("%2C");
            }
        }

        if (mHeaders.size() > 0) {
            result.append("?");
            final Iterator<Entry<String, String>> it2 = mHeaders.entrySet().iterator();
            while (it2.hasNext()) {
                final Entry<String, String> entry = it2.next();
                final String name = entry.getKey();
                final String value = entry.getValue();
                try {
                    result.append(URLEncoder.encode(name, "UTF-8") + "=" +
                            URLEncoder.encode(value, "UTF-8"));
                } catch (final Exception ex) {
                    final URISyntaxException mex =
                            new URISyntaxException(name + ":" + value, "");
                    mex.initCause(ex);
                }
                if (it2.hasNext()) {
                    result.append("&");
                }
            }
        }

        return result.toString();
    }

    private void unmarshal(final URI mailToURI) throws URISyntaxException {
        try {
            if (!MAILTO_SCHEME.equals(mailToURI.getScheme())) {
                throw new URISyntaxException(mailToURI.toString(),
                        I18n.loc("EMAILBC-7021: Cannot resolve the scheme, found \"{0}\" : expected scheme is \"{1}\"", mailToURI.getScheme(), MAILTO_SCHEME_WITH_DELIMITER));
            }

            final String data = mailToURI.getRawSchemeSpecificPart();

            // Check if there are headers
            final int questionMark = data.indexOf('?');
            if (questionMark == -1) {
                parseMailBoxes(data, "");
            } else {
                parseMailBoxes(data.substring(0, questionMark), "");
                parseHeaders(data.substring(questionMark + 1, data.length()));
            }
        } catch (final URISyntaxException ex) {
            throw ex;
        } catch (final Exception ex) {
            final URISyntaxException mex =
                    new URISyntaxException(mailToURI.toString(), ex.getMessage());
            mex.initCause(ex);
            throw mex;
        }
    }

    protected void parseMailBoxes(final String data, final String mailBoxType) {
        if ((data == null) || data.equals("")) {
            return;
        }
        String decodedData = data;
        try {
            decodedData = URLDecoder.decode(data, "US-ASCII");
        } catch (UnsupportedEncodingException ex) {
            //do nothing
        }
        final String[] mailboxes = decodedData.split(",");
        for (String element : mailboxes) {
            try {
                if (mailBoxType.compareToIgnoreCase("cc") == 0) {
                    mCCMailboxes.add(new SMTPMailbox(element));
                } else if (mailBoxType.compareToIgnoreCase("bcc") == 0) {
                    mBCCMailboxes.add(new SMTPMailbox(element));
                } else if (mailBoxType.compareToIgnoreCase("from") == 0) {
                    mFromMailbox = new SMTPMailbox(element);
                    break;
                } else {
                    mMailboxes.add(new SMTPMailbox(element));
                }
            } catch (Exception ex) {
                logger.log(Level.WARNING, ex.getMessage());
            }
        }
    }

    /**
     * Parses the headers with name (case-insensitive) "to", "cc", "bcc", "subject", "body".
     * @param data
     * @throws URISyntaxException
     */
    protected void parseHeaders(final String data)
            throws URISyntaxException {

        if ((data == null) || data.equals("")) {
            return;
        }

        final String[] entries = data.split("&");
        for (final String element : entries) {
            final String[] keyValue = element.split("=");
            if (keyValue.length != 2) {
                throw new URISyntaxException(element,
                        "Header, " + element + " , is not valid");
            }

            if ("".equals(keyValue[0])) {
                throw new URISyntaxException(keyValue[0],
                        "Header  is null ");
            }
            try {
                keyValue[0] = URLDecoder.decode(keyValue[0], "US-ASCII");
            } catch (UnsupportedEncodingException ex) {
                //do nothing
            }
            try {
                keyValue[1] = URLDecoder.decode(keyValue[1], "US-ASCII");
            } catch (UnsupportedEncodingException ex) {
                //do nothing
            }
            mHeaders.put(keyValue[0], keyValue[1]);
            try {
                if (keyValue[0].compareToIgnoreCase("cc") == 0) {
                    parseMailBoxes(keyValue[1], "cc");
                } else if (keyValue[0].compareToIgnoreCase("to") == 0) {
                    parseMailBoxes(keyValue[1], "to");
                } else if (keyValue[0].compareToIgnoreCase("bcc") == 0) {
                    parseMailBoxes(keyValue[1], "bcc");
                } else if (keyValue[0].compareToIgnoreCase("from") == 0) {
                    parseMailBoxes(keyValue[1], "from");
                } else if (keyValue[0].compareToIgnoreCase("subject") == 0) {
                    subject = keyValue[1];
                } else if (keyValue[0].compareToIgnoreCase("body") == 0) {
                    body = keyValue[1];
                } else if (keyValue[0].compareToIgnoreCase("newsgroups") == 0) {
                    newsgroups = keyValue[1];
                }
            } catch (final Exception ex) {
                throw new URISyntaxException(element,
                        "Header, " + element +
                        " , is not valid mailbox");
            }
        }

    }
}
