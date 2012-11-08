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
 * @(#)EmailMessage.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**************************************************************************
 *
 *          Copyright (c) 2003, SeeBeyond Technology Corporation,
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
package com.sun.jbi.smtpbc.extservice;

import java.util.ArrayList;

/**
 * Email message class which encapsulates the message for sending
 * and receiving email message(s).
 *
 * @author  
 * @version $Version$
 */
public class EmailMessage {
    /**
     * Creates new EmailMessage
     */
    public EmailMessage() {
        initialize();
    }

    /**
     * Initialize the internal state of the message.
     */
    protected void initialize() {
        _To = new ArrayList();
        _Cc = new ArrayList();
        _Bcc = new ArrayList();

        _From = new EmailAddress();
        _ReplyTo = new EmailAddress();

        _Subject = "";
        _MsgText = "";
        _MsgHTML = "";
        _CharSet = "";

        _AttachmentFile = new ArrayList();
    }

    protected void reset() {
        _To.clear();
        _Cc.clear();
        _Bcc.clear();

        _From.reset();
        _ReplyTo.reset();

        _Subject = "";
        _MsgText = "";
        _MsgHTML = "";
        _CharSet = "";

        _AttachmentFile.clear();
    }

    /*
     * The To list.
     */
    protected ArrayList _To;

    /**
     * Get the To EmailAddress at index i.
     *
     * @param   i   Index.
     * @return  The To EmailAddress instance at index i.
     */
    public EmailAddress getTo(final int i) {
        try {
            _To.get(i);
        } catch (final IndexOutOfBoundsException ex) {
            if ((i == 0) || (i == _To.size())) {
                addTo(new EmailAddress());
            } else {
                throw ex;   // Do not support holes.
            }
        }
        return ((EmailAddress) _To.get(i));
    }

    /**
     * Get the To EmailAddress collection as an array of EmailAddress.
     *
     * @return  An array of EmailAddress.
     */
    public EmailAddress[] getTo() {
      final EmailAddress[] tmp = new EmailAddress[_To.size()];
      _To.toArray(tmp);
      return tmp;
    }

    /**
     * Return the size of To EmailAddress collection.
     *
     * @return  The size of the To EmailAddress collection.
     */
    public int countTo() {
      return _To.size();
    }

    /**
     * Sets the To EmailAddress at index i in the collection.
     *
     * @param   i   Index.
     * @param   val The EmailAddress to set.
     */
    public void setTo(final int i, final EmailAddress val) {
      if (i == _To.size()) {
		_To.add(val);
	} else {
		_To.set(i, val);
	}
    }

    /**
     * Sets the To EmailAddress collection.
     *
     * @param   val An array of EmailAddress.
     */
    public void setTo(final EmailAddress[] val) {
      _To = new ArrayList(val.length);
      for (EmailAddress element : val) {
		_To.add(element);
	}
    }

    /**
     * Removes the To EmailAddress at index i.
     *
     * @param   i   Index.
     */
    public void removeTo(final int index) {
      _To.remove(index);
    }

    /**
     * Adds an EmailAddress to the To EmailAddress collection.
     *
     * @param   value   The EmailAddress to add.
     */
    public void addTo(final EmailAddress value) {
      _To.add(value);
    }

    /**
     * Adds an EmailAddress to the To EmailAddress collection
     * at index i.
     *
     * @param   i       The index.
     * @param   value   The EmailAddress to add.
     */
    public void addTo(final int index, final EmailAddress value) {
      _To.add(index, value);
    }

    /**
     * Clears the To EmailAddress collection.
     */
    public void clearTo() {
      _To.clear();
    }

    /*
     * The CC list.
     */
    protected ArrayList _Cc;

    /**
     * Get the CC EmailAddress at index i.
     *
     * @param   i   Index.
     * @return  The CC EmailAddress instance at index i.
     */
    public EmailAddress getCc(final int i) {
        try {
            _Cc.get(i);
        } catch (final IndexOutOfBoundsException ex) {
            if ((i == 0) || (i == _Cc.size())) {
                addCc(new EmailAddress());
            } else {
                throw ex;   // Do not support holes.
            }
        }
        return ((EmailAddress) _Cc.get(i));
    }

    /**
     * Get the CC EmailAddress collection as an array of EmailAddress.
     *
     * @return  An array of EmailAddress.
     */
    public EmailAddress[] getCc() {
      final EmailAddress[] tmp = new EmailAddress[_Cc.size()];
      _Cc.toArray(tmp);
      return tmp;
    }

    /**
     * Return the size of CC EmailAddress collection.
     *
     * @return  The size of the CC EmailAddress collection.
     */
    public int countCc() {
      return _Cc.size();
    }

    /**
     * Sets the CC EmailAddress at index i in the collection.
     *
     * @param   i   Index.
     * @param   val The EmailAddress to set.
     */
    public void setCc(final int i, final EmailAddress val) {
      if (i == _Cc.size()) {
		_Cc.add(val);
	} else {
		_Cc.set(i, val);
	}
    }

    /**
     * Sets the CC EmailAddress collection.
     *
     * @param   val An array of EmailAddress.
     */
    public void setCc(final EmailAddress[] val) {
      _Cc = new ArrayList(val.length);
      for (EmailAddress element : val) {
		_Cc.add(element);
	}
    }

    /**
     * Removes the CC EmailAddress at index i.
     *
     * @param   i   Index.
     */
    public void removeCc(final int index) {
      _Cc.remove(index);
    }

    /**
     * Adds an EmailAddress to the CC EmailAddress collection.
     *
     * @param   value   The EmailAddress to add.
     */
    public void addCc(final EmailAddress value) {
      _Cc.add(value);
    }

    /**
     * Adds an EmailAddress to the CC EmailAddress collection
     * at index i.
     *
     * @param   i       The index.
     * @param   value   The EmailAddress to add.
     */
    public void addCc(final int index, final EmailAddress value) {
      _Cc.add(index, value);
    }

    /**
     * Clears the CC EmailAddress collection.
     */
    public void clearCc() {
      _Cc.clear();
    }

    /*
     * The BCC list.
     */
    protected ArrayList _Bcc;

    /**
     * Get the BCC EmailAddress at index i.
     *
     * @param   i   Index.
     * @return  The CC EmailAddress instance at index i.
     */
    public EmailAddress getBcc(final int i) {
        try {
            _Bcc.get(i);
        } catch (final IndexOutOfBoundsException ex) {
            if ((i == 0) || (i == _Bcc.size())) {
                addBcc(new EmailAddress());
            } else {
                throw ex;   // Do not support holes.
            }
        }
        return ((EmailAddress) _Bcc.get(i));
    }

    /**
     * Get the BCC EmailAddress collection as an array of EmailAddress.
     *
     * @return  An array of EmailAddress.
     */
    public EmailAddress[] getBcc() {
      final EmailAddress[] tmp = new EmailAddress[_Bcc.size()];
      _Bcc.toArray(tmp);
      return tmp;
    }

    /**
     * Return the size of BCC EmailAddress collection.
     *
     * @return  The size of the BCC EmailAddress collection.
     */
    public int countBcc() {
      return _Bcc.size();
    }

    /**
     * Sets the BCC EmailAddress at index i in the collection.
     *
     * @param   i   Index.
     * @param   val The EmailAddress to set.
     */
    public void setBcc(final int i, final EmailAddress val) {
      if (i == _Bcc.size()) {
		_Bcc.add(val);
	} else {
		_Bcc.set(i, val);
	}
    }

    /**
     * Sets the BCC EmailAddress collection.
     *
     * @param   val An array of EmailAddress.
     */
    public void setBcc(final EmailAddress[] val) {
      _Bcc = new ArrayList(val.length);
      for (EmailAddress element : val) {
		_Bcc.add(element);
	}
    }

    /**
     * Removes the BCC EmailAddress at index i.
     *
     * @param   i   Index.
     */
    public void removeBcc(final int index) {
      _Bcc.remove(index);
    }

    /**
     * Adds an EmailAddress to the BCC EmailAddress collection.
     *
     * @param   value   The EmailAddress to add.
     */
    public void addBcc(final EmailAddress value) {
      _Bcc.add(value);
    }

    /**
     * Adds an EmailAddress to the BCC EmailAddress collection
     * at index i.
     *
     * @param   i       The index.
     * @param   value   The EmailAddress to add.
     */
    public void addBcc(final int index, final EmailAddress value) {
      _Bcc.add(index, value);
    }

    /**
     * Clears the BCC EmailAddress collection.
     */
    public void clearBcc() {
      _Bcc.clear();
    }

    /*
     * The email sender.
     */
    protected EmailAddress _From;

    /**
     * Gets the EmailAddress of the sender.
     *
     * @return  The EmailAddress of the sender.
     */
    public EmailAddress getFrom() {
      return _From;
    }

    /*
     * The ReplyTo field.
     */
    protected EmailAddress _ReplyTo;

    /**
     * Gets the EmailAddress of the ReplyTo.
     *
     * @return  The EmailAddress of the ReplyTo.
     */
    public EmailAddress getReplyTo() {
      return _ReplyTo;
    }

    /*
     * The Subject field.
     */
    protected String _Subject;

    /**
     * Gets the subject of the email message.
     *
     * @return  The subject of the email message.
     */
    public String getSubject() {
      return _Subject;
    }

    /**
     * Sets the subject of the email message.
     *
     * @param   val  The subject of the email message.
     */
    public void setSubject(final String val) {
      if (val == null) {
          _Subject = "";
      } else {
          _Subject = val;
      }

	   if(_Subject.length() == 1)
	   {
	  	    _Subject = _Subject + " ";
	   }
    }

    /*
     * The text message body.
     */
    protected String _MsgText;

    /**
     * Gets the text message of the email message body.
     *
     * @return  The text message of the email message body.
     */
    public String getMsgText() {
      return _MsgText;
    }

    /**
     * Sets the text message of the email message body.
     *
     * @param   val  The text message of the email message body.
     */
    public void setMsgText(final String val) {
      if (val == null) {
          _MsgText = "";
      } else {
          _MsgText = val;
      }
    }

    /*
     * The HTML message body.
     */
    protected String _MsgHTML;

    /**
     * Gets the html message of the email message body.
     *
     * @return  The html message of the email message body.
     */
    public String getMsgHTML() {
      return _MsgHTML;
    }

    /**
     * Sets the html message of the email message body.
     *
     * @param   val  The html message of the email message body.
     */
    public void setMsgHTML(final String val) {
      if (val == null) {
          _MsgHTML = "";
      } else {
          _MsgHTML = val;
      }
    }

    /*
     * The character encoding set.
     */
    protected String _CharSet;

    /**
     * Gets the character set encoding.
     *
     * @return  The character set encoding.
     */
    public String getCharSet() {
      return _CharSet;
    }

    /**
     * Sets the character set encoding.
     *
     * @param   val  The character set encoding.
     */
    public void setCharSet(final String val) {
      if (val == null) {
          _CharSet = "";
      } else {
          _CharSet = val;
      }
    }

    /*
     * The Attachment collection.
     */
    protected ArrayList _AttachmentFile;

    /**
     * Get the EmailAttachment at index i.
     *
     * @param   i   Index.
     * @return  The attachment at index i.
     */
    public EmailAttachment getAttachment(final int i) {
        try {
            _AttachmentFile.get(i);
        } catch (final IndexOutOfBoundsException ex) {
            if ((i == 0) || (i == _AttachmentFile.size())) {
                addAttachment(new EmailAttachment());
            } else {
                throw ex;   // Do not support holes.
            }
        }
        return ((EmailAttachment) _AttachmentFile.get(i));
    }

    /**
     * Get the EmailAttachment collection as an array of EmailAttachment.
     *
     * @return  An array of EmailAttachment.
     */
    public EmailAttachment[] getAttachment() {
      final EmailAttachment[] tmp = new EmailAttachment[_AttachmentFile.size()];
      _AttachmentFile.toArray(tmp);
      return tmp;
    }

    /**
     * Return the size of the EmailAttachment collection.
     *
     * @return  The size of the attachment collection.
     */
    public int countAttachment() {
      return _AttachmentFile.size();
    }

    /**
     * Sets the EmailAttachment at index i in the collection.
     *
     * @param   i   Index.
     * @param   val The EmailAttachment to set.
     */
    public void setAttachment(final int i, final EmailAttachment val) {
      if (i == _AttachmentFile.size()) {
		_AttachmentFile.add(val);
	} else {
		_AttachmentFile.set(i, val);
	}
    }

    /**
     * Sets the EmailAttachment collection.
     *
     * @param   val An array of EmailAttachment.
     */
    public void setAttachment(final EmailAttachment[] val) {
      _AttachmentFile = new ArrayList(val.length);
      for (EmailAttachment element : val) {
		_AttachmentFile.add(element);
	}
    }

    /**
     * Removes the EmailAttachment at index i.
     *
     * @param   i   Index.
     */
    public void removeAttachment(final int index) {
      _AttachmentFile.remove(index);
    }

    /**
     * Adds an EmailAttachment to the EmailAttachment collection.
     *
     * @param   value   The EmailAttachment to add.
     */
    public void addAttachment(final EmailAttachment value) {
      _AttachmentFile.add(value);
    }

    /**
     * Adds an EmailAttachment to the EmailAttachment collection
     * at index i.
     *
     * @param   i       The index.
     * @param   value   The EmailAttachment to add.
     */
    public void addAttachment(final int index, final EmailAttachment value) {
      _AttachmentFile.add(index, value);
    }

    /**
     * Clears the EmailAttachment collection.
     */
    public void clearAttachment() {
      _AttachmentFile.clear();
    }

}
