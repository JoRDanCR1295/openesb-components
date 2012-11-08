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
 * @(#)MailTo.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/***************************************************************************
 *
 *          Copyright (c) 2005, SeeBeyond Technology Corporation,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          SEEBEYOND TECHNOLOGY CORPORATION.
 *
 ***************************************************************************/
package com.sun.jbi.smtpbc.extensions;

import com.sun.jbi.internationalization.Messages;
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
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * The MailTo class provides an interpretation of the mailto URI syntax as per
 * RFC 2368.  The mailto URI designates an Internet mailing address of
 * individuals.  It allows for multiple senders and the setting of mail header
 * fields.
 * <p>
 * Encoding is a very tricky part of the URI syntax.  The MailTo class only
 * accepts properly encoded strings.  The unmarshal() method will fail if
 * given an improper encoding.  The marshal() method only returns encoded
 * strings.  Once a mailto URI is unmarshalled, calls to methods getMailBox(),
 * addMailbox(), addHeader(), getHeader(), and getHeaders() will return
 * unencoded strings.  Using encoded strings with these methods will result
 * in unpredicatable behavior if the MailTo class is unmarshalled.  The
 * reasoning for this decision is that most
 * users of the class only have to deal with encodings at the unmarshal and
 * marshal level.  Most times they don't want to have to deal with it when
 * working with the components of the mailto URI
 * <p>
 * The encoding scheme is defined by RFC 2368 and RFC 1738, but it is detailed
 * below.
 * <ul>
 * <li>All unsafe characters as defined by RFC 1738 must be encoded.  For the
 * mailto URI this includes common characters such as the space, the quote, the
 * less-than-sign, and the greater-than-sign.</li>
 * <li>All alphanumeric characters do NOT have to be encoded.</li>
 * <li>All special characters $-_.+!*'(), do NOT have to be encoded.</li>
 * <li>The question mark (?), the equals sign (=), and the ampersand sign (&) are
 * reserved characters in the mailto URI scheme.  That means if they are used in
 * a mailbox or header, they must be encoded.
 * <p>
 * <blockquote>
 *       foo@domain.com?someHeader=blah&blah     // WRONG!
 *
 *       foo@domain.com?someHeader=blah%26blah   // RIGHT
 * </blockquote>
 * </li>
 * <li>All other URL reserved characters that are not being used for the mailto
 * URI scheme do NOT have to be encoded.  This includes semi-colon (;),
 * slash (/), colon (:), and the at-sign ("@").
 * </ul>
 *
 * @author       Alexander Fung
 * @version      
 *
 */
public class MailTo {
    
    public static final String MAILTO_SCHEME = "mailto";
    
    private Map mHeaders;
    private Collection mMailboxes;
    private Collection mCCMailboxes;
    private Collection mBCCMailboxes;
    private String subject;
    private String body;
    
    private static final Messages mMessages =
                Messages.getMessages(MailTo.class);
    
    private static final Logger mLogger =
                Messages.getLogger(MailTo.class);
    public MailTo() {
        mHeaders = new HashMap();
        mMailboxes = new ArrayList();
        mCCMailboxes = new ArrayList();
        mBCCMailboxes = new ArrayList();
        subject = "";
        body = "";
    }
    
    public MailTo(final String mailToURI) throws URISyntaxException {
        this();
        unmarshal(new URI(mailToURI));
    }
    
    public String getProtocol() {
        return MailTo.MAILTO_SCHEME;
    }
    
    public Collection getMailbox() {
        return mMailboxes;
    }
    
     public Collection getCCMailbox() {
        return mCCMailboxes;
    }
     
      public Collection getBCCMailbox() {
        return mBCCMailboxes;
    }
    
    public void addMailbox(final Mailbox mailbox) {
        mMailboxes.add(mailbox);
    }
    
    public void addHeader(final String name, final String value) {
        mHeaders.put(name, value);
    }
    
    public String getHeader(final String name) {
        return (String)mHeaders.get(name);
    }
    
    public Map getHeaders() {
        return mHeaders;
    }
    
    public String getSubject(){
        return subject;
    }
    public String getBody(){
        return body;
    }
    
    public String marshal() throws URISyntaxException {
        final StringBuffer result = new StringBuffer();
        result.append(MailTo.MAILTO_SCHEME + ":");
        final Iterator it = mMailboxes.iterator();
        while (it.hasNext()) {
            final Mailbox mailBox = (Mailbox)it.next();
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
            final Iterator it2 = mHeaders.entrySet().iterator();
            while (it2.hasNext()) {
                final Map.Entry entry = (Map.Entry)it2.next();
                final String name = (String)entry.getKey();
                final String value = (String)entry.getValue();
                try {
                    result.append(URLEncoder.encode(name, "UTF-8") + "=" +
                            URLEncoder.encode(value, "UTF-8"));
                } catch (final Exception ex) {
                    final URISyntaxException mex =
                            new URISyntaxException(name+":"+value, "");
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
    	if(mLogger.isLoggable(Level.INFO)){
    		mLogger.log(Level.INFO,mMessages.getString("MailTo.unmarshalling",mailToURI));
    	}
        try {
        	
            if (mailToURI.getScheme().equals(MailTo.MAILTO_SCHEME + ":")) {
                throw new URISyntaxException(mailToURI.toString(),
                        mMessages.getString("MailTo.invalidScheme",mailToURI.getScheme()));
            }
            
            final String data = mailToURI.getRawSchemeSpecificPart();
            
            // Check if there are headers
            final int questionMark = data.indexOf('?');
            if (questionMark == -1) {
                parseMailBoxes(data,"");
            } else {
                parseMailBoxes(data.substring(0, questionMark),"");
                parseHeaders(data.substring(questionMark + 1,
                        data.length()));
            }
        } catch (final URISyntaxException ex) {
            throw ex;
        }catch (final Exception ex) {
            final URISyntaxException mex =
                    new URISyntaxException(mailToURI.toString(), ex.getMessage());
            mex.initCause(ex);
            throw mex;
        }
    }
    
    protected void parseMailBoxes(final String data, final String mailBoxType)
    throws InvalidMailboxException {
        
        if ((data == null) || data.equals("")) {
            return;
        }
        String decodedData = data;
        try {
             decodedData = URLDecoder.decode(data,"US-ASCII");
        } catch (UnsupportedEncodingException ex) {
            //do nothing
        }
        final String[] mailboxes = decodedData.split(",");
        for (String element : mailboxes) {
            try {
                if (mailBoxType.compareToIgnoreCase("cc")==0) {
					mCCMailboxes.add(new Mailbox(element));
				} else if (mailBoxType.compareToIgnoreCase("bcc")==0 ) {
					mBCCMailboxes.add(new Mailbox(element));
				} else {
					mMailboxes.add(new Mailbox(element));
				}
            } catch (InvalidMailboxException ex) {
                mLogger.log(Level.WARNING,ex.getMessage());
            }
        }
    }
    
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
                        "Header, " + element +
                        " , is not valid");
            }
            
            if("".equals(keyValue[0])){
                throw new URISyntaxException(keyValue[0],
                        "Header  is null ");
            }
            try {
                keyValue[0] = URLDecoder.decode(keyValue[0],"US-ASCII");
            } catch (UnsupportedEncodingException ex) {
                //do nothing
            }
            try {
                keyValue[1] = URLDecoder.decode(keyValue[1],"US-ASCII");
            } catch (UnsupportedEncodingException ex) {
                //do nothing
            }
            mHeaders.put(keyValue[0], keyValue[1]);
            try{
                if(keyValue[0].compareToIgnoreCase("cc")== 0) {
					parseMailBoxes(keyValue[1], "cc");
                }else if (keyValue[0].compareToIgnoreCase("to")== 0){
                	parseMailBoxes(keyValue[1], "to");
				} else if (keyValue[0].compareToIgnoreCase("bcc")== 0) {
					parseMailBoxes(keyValue[1], "bcc");
				} else if (keyValue[0].compareToIgnoreCase("subject") == 0){
                                        subject = keyValue[1];
                                } else if (keyValue[0].compareToIgnoreCase("body") == 0){
                                        body = keyValue[1];
                                }
            } catch (final InvalidMailboxException ex) {
                throw new URISyntaxException(element,
                        "Header, " + element +
                        " , is not valid mailbox");
            }
        }
        
    }
    
}
