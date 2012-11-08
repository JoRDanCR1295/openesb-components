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
 * @(#)EmailClientAgent.java 
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

import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.activation.DataHandler;
import javax.mail.AuthenticationFailedException;
import javax.mail.Flags;
import javax.mail.Folder;
import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Store;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;
import javax.mail.internet.MimePart;
import javax.mail.internet.MimeUtility;
import javax.mail.internet.ParseException;
import com.sun.jbi.internationalization.Messages;
import java.security.Security; 

// import com.stc.connector.emailadapter.alerts.EmailAlertCodes;
// import com.stc.eventmanagement.NotificationEvent;

/**
 * APIS for checking email, receiving email, and sending email.
 * 
 * @author 
 * @version $Version$
 */
public class EmailClientAgent {

	private boolean pop3HasMore = false;

	private static final Logger mLogger = Messages.getLogger(EmailClientAgent.class);
        
        
	/**
	 * Constructor for EmailClientAgent.
	 * 
	 * @param mbeanObject
	 *            The MBean object used for sending alerts.
	 */
	public EmailClientAgent() {
	}

	/**
	 * Process out the recipients of this message by parsing the addresses. When
	 * <code>dirIn</code> is <code>true</code> process incoming message,
	 * otherwise outgoing.
	 * 
	 * @param msg
	 *            The MimeMessage which contains the email message from the
	 *            server.
	 * @param type
	 *            The RecipientType to process.
	 * @param dirIn
	 *            The email message flow (receive or send).
	 * @param emailMsg
	 *            The EmailMessage instance to populate in the case of receive
	 *            or to read from in the case of send.
	 * 
	 * @throws EmailApplicationException
	 *             upon error.
	 */
	protected void processRecipients(final MimeMessage msg,
			final Message.RecipientType type, final boolean dirIn, final EmailMessage emailMsg)
			throws EmailApplicationException {
		ArrayList recipients;
		if (type.equals(Message.RecipientType.TO)) {
			recipients = emailMsg._To;
		} else if (type.equals(Message.RecipientType.CC)) {
			recipients = emailMsg._Cc;
		} else if (type.equals(Message.RecipientType.BCC)) {
			recipients = emailMsg._Bcc;
		} else {
			return;
		}

		try {
			if (dirIn) {
				// Process incoming message
				final InternetAddress[] iaddr = (InternetAddress[]) msg
						.getRecipients(type);
				if (null != iaddr) {
					for (InternetAddress element : iaddr) {
						recipients.add(new EmailAddress(element.getAddress(),
								element.getPersonal()));
					}
				}
			} else {
				// Process outgoing message
				EmailAddress ea = null;
				try {
					final InternetAddress[] addra = new InternetAddress[recipients
							.size()];
					if (emailMsg._CharSet.length() > 0) {
						for (int i = 0; i < recipients.size(); i++) {
							ea = (EmailAddress) recipients.get(i);
							addra[i] = new InternetAddress(ea.getAddress(), ea
									.getName(), emailMsg._CharSet);
						}
					} else {
						for (int i = 0; i < recipients.size(); i++) {
							ea = (EmailAddress) recipients.get(i);
							addra[i] = new InternetAddress(ea.getAddress(), ea
									.getName());
						}
					}
					msg.setRecipients(type, addra);
				} catch (final UnsupportedEncodingException ex) {
					final String str = "EmailClientAgent.processRecipients():\n"
							+ "Unable to create InternetAddress\n" + "Address="
							+ ea.getAddress() + ", Name=" + ea.getName()
							+ ",\n" + "CharSet=" + emailMsg._CharSet + ",\n"
							+ "Exception info: " + ex.toString();
					throw new EmailApplicationException(str);
				}
			}
		} catch (final MessagingException ex) {
			final String str = "EmailClientAgent.processRecipients():\n"
					+ "Error occurred while processing "
					+ ((dirIn) ? "inbound" : "outbound") + " message,\n"
					+ "Exception info: " + ex.toString();
			throw new EmailApplicationException(str);
		}
	}

	/**
	 * Process out the MsgText and MsgHTML parts of the message.
	 * 
	 * @param container
	 *            The MimePart of the mime message.
	 * @param emailMsg
	 *            The EmailMessage containing the email message to be sent.
	 * 
	 * @throws EmailApplicationException
	 *             upon error.
	 */
	protected void processOutMsgTextHTML(final MimePart container,
			final EmailMessage emailMsg) throws EmailApplicationException {
		try {
			if (emailMsg._MsgHTML.length() > 0) {
				mLogger
						.fine("Send message multipart/alternative; sending MsgText and MsgHTML");
				// We have to compose a new MimeMultipart("alternative")
				final MimeMultipart mpa = new MimeMultipart("alternative");
				container.setContent(mpa);
				// Set the text part
				final MimeBodyPart mbpMsgText = new MimeBodyPart();
				mpa.addBodyPart(mbpMsgText);
				final String text = (emailMsg._MsgText.length() > 0) ? emailMsg._MsgText
						: "This message contains an alternative MIME part in HTML format.";
				if (emailMsg._CharSet.length() > 0) {
					mbpMsgText.setText(text, emailMsg.getCharSet());
				} else {
					mbpMsgText.setText(text);
				}
				mLogger.fine("Send message; MsgText [" + text + "]");
				// Set the HTML part
				final MimeBodyPart mbpMsgHTML = new MimeBodyPart();
				mpa.addBodyPart(mbpMsgHTML);
				if (emailMsg._CharSet.length() > 0) {
					mbpMsgHTML.setContent(emailMsg._MsgHTML,
							"text/html; charset=\"" + emailMsg.getCharSet()
									+ "\"");
				} else {
					mbpMsgHTML.setContent(emailMsg._MsgHTML, "text/html");
				}
				mLogger.fine("Send message; MsgHTML [" + emailMsg._MsgHTML
						+ "]");
			} else {
				mLogger.fine("Send text/plain message; send MsgText");
				// Set the text part
				if (emailMsg._CharSet.length() > 0) {
					container.setText(emailMsg._MsgText, emailMsg.getCharSet());
				} else {
					container.setText(emailMsg._MsgText);
				}
				mLogger.fine("Send message; MsgText [" + emailMsg._MsgText
						+ "]");
			}
		} catch (final MessagingException ex) {
			final String str = "EmailClientAgent.processOutMsgTextHTML():\n"
					+ "Unable to process either text or HTML part of the message,\n"
					+ "Exception info: " + ex.toString();
			throw new EmailApplicationException(str);
		}
	}

	/**
	 * Process out the Attachment parts of the message
	 * 
	 * @param container
	 *            The MimeMultipart message.
	 * @param emailMsg
	 *            The EmailMessage instance to hold the email message.
	 * 
	 * @throws EmailApplicationException
	 *             upon error.
	 */
	protected void processOutAttachment(final MimeMultipart container,
			final EmailMessage emailMsg) throws EmailApplicationException {
		int i = -1;
		try {
			mLogger.fine("Process out attachment(s); attachment size ["
					+ emailMsg._AttachmentFile.size() + "]");

			// Create separate part for each attachment and assign the
			// appropriate
			// DataSource and DataHandler objects
			for (i = 0; i < emailMsg._AttachmentFile.size(); i++) {
				final MimeBodyPart mpatt = new MimeBodyPart();
				container.addBodyPart(mpatt);
				final EmailAttachment attf = (EmailAttachment) emailMsg._AttachmentFile
						.get(i);
				final AttachmentDataSource fds = new AttachmentDataSource(attf);
				mpatt.setDataHandler(new DataHandler(fds));
				final String filename = fds.getName();
				mLogger.fine("Attachment filename [" + filename + "]; "
						+ "contentType [" + fds.getContentType() + "]");
				if (filename == null) {
					throw new EmailApplicationException(
							"Attachment filename for attachment #" + i
									+ " must be specified.");
				}
				if (emailMsg._CharSet.length() > 0) {
					try {
						mpatt.setFileName(MimeUtility.encodeWord(filename,
								emailMsg.getCharSet(), "B"));
					} catch (final Exception ex) {
						final String str = "EmailClientAgent.processOutAttachment():\n"
								+ "Unsupported Encoding = "
								+ emailMsg.getCharSet()
								+ "\n"
								+ "Exception info: " + ex.toString();
						throw new EmailApplicationException(str);
					}
				} else {
					mpatt.setFileName(filename);
				}
			}
		} catch (final MessagingException ex) {
			final EmailAttachment attf = (EmailAttachment) emailMsg._AttachmentFile
					.get(i);
			final String str = "EmailClientAgent.processOutAttachment():\n"
					+ "Unable to send attachment number=" + i + ", name="
					+ attf.getName() + "Exception info: " + ex.toString();
			throw new EmailApplicationException(str);
		}
	}

	/**
	 * Process in the message specified in <code>msg</code>
	 * 
	 * @param msg
	 *            The MimeMessage which contains the email message from the
	 *            server.
	 * @param emailMsg
	 *            The EmailMessage instance to store the results from the
	 *            server.
	 * 
	 * @throws EmailApplicationException
	 *             upon error.
	 */
	protected void processInMessage(final MimeMessage msg, final EmailMessage emailMsg)
			throws EmailApplicationException {
		/**
		 * We will handle the following cases: Case One: The message is
		 * text/plain only. Case Two: The message is text/html only. Case Three:
		 * The message content is multipart/alternative, which contains
		 * text/plain and text/html parts Case Four: The message content is
		 * multipart/mixed, which in turn contains Cases One-Three as its first
		 * part plus attachments as subsequent MIME parts.
		 */
		try {
			// Extract the message basics
			InternetAddress[] iaddr = null;
			iaddr = (InternetAddress[]) msg.getFrom();
			if (null != iaddr) {
				emailMsg._From.setAddress(iaddr[0].getAddress());
				emailMsg._From.setName(iaddr[0].getPersonal());
			}
			iaddr = (InternetAddress[]) msg.getReplyTo();
			if (null != iaddr) {
				emailMsg._ReplyTo.setAddress(iaddr[0].getAddress());
				emailMsg._ReplyTo.setName(iaddr[0].getPersonal());
			}
			emailMsg.setSubject(msg.getSubject());
			processRecipients(msg, Message.RecipientType.TO, true, emailMsg);
			processRecipients(msg, Message.RecipientType.CC, true, emailMsg);
			processRecipients(msg, Message.RecipientType.BCC, true, emailMsg);
			mLogger.fine("Charset:" + emailMsg._CharSet);
			if (processInSimpleMessage(msg, emailMsg)) {
				/**
				 * No multiple MIME parts, multipart/alternative at most. We are
				 * done with it.
				 */
				mLogger
						.fine("No multiple MIME parts, multipart/alternative at most. "
								+ "We are done with it.");
			} else if (msg.isMimeType("multipart/mixed")) {
				mLogger.fine("Multipart/mixed message.");
				// Handle a complex message
				final MimeMultipart mpm = (MimeMultipart) msg.getContent();
				MimeBodyPart mbp = null;
				EmailAttachment fa = null;
				InputStream is = null;
				String fn = null;
				String contentType = null;
				final boolean doneWithZero = processInSimpleMessage(
						(MimeBodyPart) mpm.getBodyPart(0), emailMsg);
				for (int i = (doneWithZero) ? 1 : 0; i < mpm.getCount(); i++) {
					// Get the attachments and store them in memory
					mbp = (MimeBodyPart) mpm.getBodyPart(i);
					is = mbp.getInputStream();
					if (null != is) {
						fa = new EmailAttachment();
						fn = mbp.getFileName();
						if ((null != fn) && (fn.length() > 0)) {
							try {
								fn = MimeUtility.decodeText(fn);

							} catch (final UnsupportedEncodingException ex) {
								mLogger
										.warning("UnsupportedEncodingException for file attachment ("
												+ fn
												+ "}: Exception info: "
												+ ex.toString());
							}
						}

						if ((null == fn) || (fn.length() <= 0)) {
							fn = "Attachment" + i;
						}
						contentType = mbp.getContentType();
						if ((contentType == null) || (contentType.length() <= 0)) {
							contentType = "application/octet-stream";
						}
						fa.setName(fn);
						fa.readFrom(is);
						fa.setContentType(contentType);
						emailMsg.addAttachment(fa);
					}
				}
			} else {
				mLogger.fine("Treating as attachment");
				// Treat it as attachment.
				final InputStream is = msg.getInputStream();
				if (null != is) {
					final EmailAttachment fa = new EmailAttachment();

					String fn = null;
					final String aFileName = msg.getFileName();
					if ((null != aFileName) && (aFileName.length() > 0)) {
						try {
							fn = MimeUtility.decodeText(aFileName);

						} catch (final UnsupportedEncodingException ex) {
							mLogger
									.warning("UnsupportedEncodingException for file attachment ("
											+ aFileName
											+ "}: Exception info: "
											+ ex.toString());
						}
					}

					String contentType = null;
					if ((null == fn) || (fn.length() <= 0)) {
						fn = "Attachment" + 0;
					}
					contentType = msg.getContentType();
					if ((contentType == null) || (contentType.length() <= 0)) {
						contentType = "application/octet-stream";
					}
					fa.setName(fn);
					fa.readFrom(is);
					fa.setContentType(contentType);
					emailMsg.addAttachment(fa);
				}
			}
		} catch (final MessagingException ex) {
			final String str = "EmailClientAgent.processInMessage():\n"
					+ "Messaging Exception occurred while parsing message content,\n"
					+ "Exception info: " + ex.toString();
			throw new EmailApplicationException(str);
		} catch (final IOException ex) {
			final String str = "EmailClientAgent.processInMessage():\n"
					+ "IO Exception occurred while receiving message content,\n"
					+ "Exception info: " + ex.toString();
			throw new EmailApplicationException(str);
		}
	}

	/**
	 * Process in the simple message specified in <code>part</code>
	 * 
	 * @param part
	 *            The message MimePart.
	 * @param emailMsg
	 *            The EmailMessage instance to populate.
	 * 
	 * @throws EmailApplicationException
	 *             upon error.
	 */
	protected boolean processInSimpleMessage(final MimePart part,
			final EmailMessage emailMsg) throws EmailApplicationException {
		/**
		 * We will handle the following cases: Case One: The message is
		 * text/plain only. Case Two: The message is text/html only. Case Three:
		 * The message content is multipart/alternative, which contains
		 * text/plain and text/html parts
		 */
		boolean ret = false;
		try {
			if (part.isMimeType("text/plain")) {
				final String msgText = (String) part.getContent();
				emailMsg.setMsgText(msgText);
				mLogger
						.fine("Receive simple message; MimeType is text/plain; MsgText ["
								+ msgText + "]");
				ret = true;
			} else if (part.isMimeType("text/html")) {
				final String msgHTML = (String) part.getContent();
				emailMsg.setMsgHTML(msgHTML);
				mLogger
						.fine("Receive simple message; MimeType is text/html; MsgHTML ["
								+ msgHTML + "]");
				ret = true;
			} else if (part.isMimeType("multipart/alternative")) {
				final MimeMultipart mpa = (MimeMultipart) part.getContent();
				for (int i = 0; i < mpa.getCount(); i++) {
					// TODO : Wouldn't this over-write??
					if (mpa.getBodyPart(i).isMimeType("text/plain")) {
						final String msgText = (String) mpa.getBodyPart(i)
								.getContent();
						emailMsg.setMsgText(msgText);
						mLogger
								.fine("Receive multipart/alternative message; body MimeType is text/plain; MsgText ["
										+ msgText + "]");
					} else if (mpa.getBodyPart(i).isMimeType("text/html")) {
						final String msgHTML = (String) mpa.getBodyPart(i)
								.getContent();
						emailMsg.setMsgHTML(msgHTML);
						mLogger
								.fine("Receive multipart/alternative message; body MimeType is text/html; MsgHTML ["
										+ msgHTML + "]");
					} else {
						final String str = "EmailClientAgent.processInSimpleMessage():\n"
								+ "Unsupported content type in multipart/alternative: "
								+ mpa.getBodyPart(i).getContentType();
						throw new EmailApplicationException(str);
					}
				}
				ret = true;
			} else {
				// Not a simple message
				ret = false;
			}
		} catch (final MessagingException ex) {
			final String str = "EmailClientAgent.processInSimpleMessage():\n"
					+ "Messaging Exception occurred while parsing message content,\n"
					+ "Exception info: " + ex.toString();
			throw new EmailApplicationException(str);
		} catch (final IOException ex) {
			final String str = "EmailClientAgent.processInSimpleMessage():\n"
					+ "IO Exception occurred while receiving message content,\n"
					+ "Exception info: " + ex.toString();
			throw new EmailApplicationException(str);
		}
		return ret;
	}

	/**
	 * Send the message specified in the end user properties.
	 * 
	 * @param configuration
	 *            The EmailConfiguration instance which holds the connection
	 *            specific information.
	 * @param emailMsg
	 *            The EmailMessage instance which contains the email message to
	 *            be sent.
	 * 
	 * @return True if send was successful; false otherwise.
	 * 
	 * @throws EmailApplicationException
	 *             upon error.
	 */
	public boolean sendMessage(final EmailConfiguration configuration,
			final EmailMessage emailMsg) throws EmailApplicationException {
		boolean retVal = false;

		if (configuration == null) {
			final String str = "EmailClientAgent.sendMessage():\n"
					+ "Configuration for email is not provided; it is null";
			mLogger.severe(str);
			throw new EmailApplicationException(str);
		}

		if (emailMsg == null) {
			final String str = "EmailClientAgent.sendMessage():\n"
					+ "EmailMessage is not provided; it is null";
			mLogger.severe(str);
			throw new EmailApplicationException(str);
		}
                
		// Debug Log email message
		if (mLogger.isLoggable(Level.FINE)) {
			final StringBuffer buff = new StringBuffer();
			buff.append(System.getProperty("line.separator"));
			buff.append("To: ");
			for (int i = 0; i < emailMsg._To.size(); i++) {
				final EmailAddress addr = (EmailAddress) emailMsg._To.get(i);
				final String name = (addr.getName() == null ? "" : ("("
						+ addr.getName() + ")"));
				buff.append(addr.getAddress() + name);
				if (i < (emailMsg._To.size() - 1)) {
					buff.append("; ");
				}
			}
			buff.append(System.getProperty("line.separator"));

			buff.append("Cc: ");
			for (int i = 0; i < emailMsg._Cc.size(); i++) {
				final EmailAddress addr = (EmailAddress) emailMsg._Cc.get(i);
				final String name = (addr.getName() == null ? "" : ("("
						+ addr.getName() + ")"));
				buff.append(addr.getAddress() + name);
				if (i < (emailMsg._Cc.size() - 1)) {
					buff.append("; ");
				}
			}
			buff.append(System.getProperty("line.separator"));

			buff.append("Bcc: ");
			for (int i = 0; i < emailMsg._Bcc.size(); i++) {
				final EmailAddress addr = (EmailAddress) emailMsg._Bcc.get(i);
				final String name = (addr.getName() == null ? "" : ("("
						+ addr.getName() + ")"));
				buff.append(addr.getAddress() + name);
				if (i < (emailMsg._Bcc.size() - 1)) {
					buff.append("; ");
				}
			}
			buff.append(System.getProperty("line.separator"));

			String name = (emailMsg._From.getName() == null ? "" : ("("
					+ emailMsg._From.getName() + ")"));
			buff.append("From: " + emailMsg._From.getAddress() + name);
			buff.append(System.getProperty("line.separator"));

			name = (emailMsg._ReplyTo.getName() == null ? "" : ("("
					+ emailMsg._ReplyTo.getName() + ")"));
			buff.append("ReplyTo: " + emailMsg._ReplyTo.getAddress() + name);
			buff.append(System.getProperty("line.separator"));

			buff.append("Subject: " + emailMsg._Subject);
			buff.append(System.getProperty("line.separator"));

			buff.append("MsgTxt [" + emailMsg.getMsgText() + "]");
			buff.append(System.getProperty("line.separator"));

			buff.append("MsgHTML [" + emailMsg.getMsgHTML() + "]");
			buff.append(System.getProperty("line.separator"));

			buff.append("CharSet [" + emailMsg.getCharSet() + "]");
			buff.append(System.getProperty("line.separator"));

			mLogger.fine(buff.toString());
		}

		// Check for required fields
		/*if (emailMsg._To.size() == 0) {
			final String str = "EmailClientAgent.sendMessage():\n"
					+ "At least one To recipient must be specified.";
			mLogger.severe(str);
			throw new EmailApplicationException(str);
		}*/

		if (emailMsg._From.getAddress() == null) {
			final String str = "EmailClientAgent.sendMessage():\n"
					+ "The From address must be specified.";
			mLogger.severe(str);
			throw new EmailApplicationException(str);
		}

		/**
		 * ... to compose and send the message We have the following content
		 * cases to handle: Case One: MsgText - message content text/plain Case
		 * Two: Text+HTML or HTML only (plus dummy text) - message content
		 * multipart/alternative Case Three: Attachment has elements - message
		 * content miltipart/mixed and the first part contains Case One or Case
		 * Two.
		 */

		// Test for the required elements
		emailMsg._To.trimToSize();
		emailMsg._Cc.trimToSize();
		emailMsg._Bcc.trimToSize();
		emailMsg._AttachmentFile.trimToSize();
		//if ((emailMsg._To.size() == 0)
			//	|| (emailMsg._From.getAddress().length() == 0))
		if (emailMsg._From.getAddress().length()== 0){
			final String str = "EmailClientAgent.sendMessage():\n"
					+ "Can not send the message, because required fields are missing,\n"
					+ "From=" + emailMsg._From + ", To.size="
					+ emailMsg._To.size();
			mLogger.severe(str);
			throw new EmailApplicationException(str);
		}
                 Session session = null;
                 if(configuration.getUseSSL()){
                           
                 Security.addProvider(new com.sun.net.ssl.internal.ssl.Provider());
	         System.setProperty("java.protocol.handler.pkgs","com.sun.net.ssl.internal.www.protocol");
                 session = Session.getInstance(configuration.getProperties(),configuration.getAuthenticator());
		 mLogger.fine("Got session instance for ssl based Auth.");
                 }else{
                 session = Session.getInstance(configuration.getProperties(),configuration.getAuthenticator());
		 mLogger.fine("Got session instance.");
                 }
                 if(mLogger.isLoggable(Level.FINE)) {
			session.setDebug(true);
                 }

		 if (configuration.getSessionAuth()) {
			Store store = null;
			/**
			 * We have to perform POP3 authentication first.
			 */
			mLogger
					.fine("Send requires session authentication, performing session authentication first.");
			try {
				store = session.getStore();
				if (null == store) {
					mLogger.severe("Unable to get message store for this session.");
					throw new EmailApplicationException(
							"Unable to get message store for this session.");
				}
				mLogger.fine("getStore succeeded from session instance.");
				mLogger.fine("Connecting to store...");
				try {
					store.connect();
				} catch (final AuthenticationFailedException authEx) {
					mLogger.severe(authEx.getMessage());
					// sendAlert (EmailAlertCodes.EMAILEWAY_CONNECT_FAILED,
					// new String[] {configuration.getHostRecv(),
					// configuration.getProperties().getProperty("mail.store.protocol"),
					// Integer.toString(configuration.getPortRecv())},
					// "Sending e-mail requires authentication; unable to
					// authenticate user " +
					// configuration.getUserRecv() + ".",
					// NotificationEvent.SEVERITY_TYPE_CRITICAL);
					throw authEx;
				} catch (final MessagingException msgEx) {
					mLogger.severe(msgEx.getMessage());
					// sendAlert (EmailAlertCodes.EMAILEWAY_CONNECT_FAILED,
					// new String[] {configuration.getHostRecv(),
					// configuration.getProperties().getProperty("mail.store.protocol"),
					// Integer.toString(configuration.getPortRecv())},
					// "Sending e-mail requires authentication; unable to
					// connect due to MessagingException. " +
					// "Please see log file for details.",
					// NotificationEvent.SEVERITY_TYPE_CRITICAL);
					throw msgEx;
				} finally {
					if (store != null) {
						try {
							store.close();
						} catch (final MessagingException ex) {
							mLogger.severe(ex.getMessage());
							final String str = "EmailClientAgent.sendMessage():\n"
									+ "Unsuccessful POP3 authentication,\n"
									+ "Exception info: " + ex.toString();
							throw new EmailApplicationException(
									"Unable to close message store for this session: "
											+ str);
						}
					}
				}
				// store.close(); // Move to finally to ensure it inbox will not
				// be locked in error condition
				mLogger.fine("Connect to store succeeded.");
			} catch (final MessagingException ex) {
				mLogger.severe(ex.getMessage());
				final String str = "EmailClientAgent.sendMessage():\n"
						+ "Unsuccessful POP3 authentication,\n"
						+ "Exception info: " + ex.toString();
				throw new EmailApplicationException(
						"Unable to get message store for this session: " + str);
			} catch (final Exception ex) {
				mLogger.severe(ex.getMessage());
				final String str = "EmailClientAgent.sendMessage():\n"
						+ "Unsuccessful POP3 authentication,\n"
						+ "Exception info: " + ex.toString();
				// throw new EmailApplicationException ("Unable to get message
				// store for this session.");
				throw new EmailApplicationException(
						"Unable to get message store for this session: " + str);
			}
		}

		try {
			final MimeMessage msg = new MimeMessage(session);
			// Set up the message basics
			if (emailMsg._CharSet.length() > 0) {
				mLogger.fine("Setting MimeMessage Subject field to "
						+ emailMsg.getSubject());
				msg.setSubject(emailMsg.getSubject(), emailMsg._CharSet);
				mLogger.fine("Setting MimeMessage From field");
				msg.setFrom(new InternetAddress(emailMsg._From.getAddress(),
						emailMsg._From.getName(), emailMsg._CharSet));
				if (emailMsg._ReplyTo.getAddress().length() > 0) {
					mLogger.fine("Setting MimeMessage ReplyTo field");
					msg.setReplyTo(new InternetAddress[] { new InternetAddress(
							emailMsg._ReplyTo.getAddress(), emailMsg._ReplyTo
									.getName(), emailMsg._CharSet) });
				}
			} else {
				mLogger.fine("Setting MimeMessage Subject field to "
						+ emailMsg.getSubject());
				msg.setSubject(emailMsg.getSubject());
				mLogger.fine("Setting MimeMessage From field");
				msg.setFrom(new InternetAddress(emailMsg._From.getAddress(),
						emailMsg._From.getName()));
				if (emailMsg._ReplyTo.getAddress().length() > 0) {
					mLogger.fine("Setting MimeMessage ReplyTo field");
					msg.setReplyTo(new InternetAddress[] { new InternetAddress(
							emailMsg._ReplyTo.getAddress(), emailMsg._ReplyTo
									.getName()) });
				}
			}

			mLogger.fine("Processing To recipients...");
			processRecipients(msg, Message.RecipientType.TO, false, emailMsg);
			mLogger.fine("Processing Cc recipients...");
			processRecipients(msg, Message.RecipientType.CC, false, emailMsg);
			mLogger.fine("Processing Bc recipients...");
			processRecipients(msg, Message.RecipientType.BCC, false, emailMsg);

			final Date sentDate = new Date();
			mLogger.fine("Setting sentDate field to " + sentDate.toString());
			msg.setSentDate(sentDate);

			mLogger
					.fine("Setting X-Mailer header field to SeeBeyond Java E-mail e*Way for e*Gate");
			msg.setHeader("X-Mailer", "SeeBeyond Java E-mail e*Way for e*Gate");

			// Now set up the message structure
			if (emailMsg._AttachmentFile.size() > 0) {
				mLogger.fine("Processing attachments and message body...");
				// Create a new MimeMultipart("mixed") to hold our message
				// content
				final MimeMultipart mpm = new MimeMultipart("mixed");
				msg.setContent(mpm);
				// Create a new MimeBodyPart() to hold the MsgTextHTML
				final MimeBodyPart mbpMsgTextHTML = new MimeBodyPart();
				mpm.addBodyPart(mbpMsgTextHTML);
				processOutMsgTextHTML(mbpMsgTextHTML, emailMsg);
				processOutAttachment(mpm, emailMsg);
			} else {
				mLogger.fine("No attachments; processing message body...");
				processOutMsgTextHTML(msg, emailMsg);
			}

			Transport.send(msg);
			mLogger.info("Email message sent successfully.");

		} catch (final ParseException ex) {
			mLogger.severe(ex.getMessage());
			// sendAlert (EmailAlertCodes.EMAILEWAY_SEND_FAILED,
			// new String[] {configuration.getHostSend(),
			// Integer.toString(configuration.getPortSend())},
			// "Unable to send e-mail message due to ParseException. Please see
			// log file for details.",
			// NotificationEvent.SEVERITY_TYPE_CRITICAL);
			final String str = "EmailClientAgent.sendMessage():\n"
					+ "Unable to send message, invalid content type?\n"
					+ "Exception info: " + ex.toString();
			throw new EmailApplicationException(str);
		} catch (final MessagingException ex) {
			mLogger.severe(ex.getMessage());
			// sendAlert (EmailAlertCodes.EMAILEWAY_SEND_FAILED,
			// new String[] {configuration.getHostSend(),
			// Integer.toString(configuration.getPortSend())},
			// "Unable to send e-mail message due to MessagingException. Please
			// see log file for details.",
			// NotificationEvent.SEVERITY_TYPE_CRITICAL);
			final String str = "EmailClientAgent.sendMessage():\n"
					+ "Unable to send message,\n" + "Exception info: "
					+ ex.toString();
			throw new EmailApplicationException(str);
		} catch (final UnsupportedEncodingException ex) {
			mLogger.severe(ex.getMessage());
			// sendAlert (EmailAlertCodes.EMAILEWAY_SEND_FAILED,
			// new String[] {configuration.getHostSend(),
			// Integer.toString(configuration.getPortSend())},
			// "Unable to send e-mail message due to
			// UnsupportedEncodingException. Please see log file for details.",
			// NotificationEvent.SEVERITY_TYPE_CRITICAL);
			final String str = "EmailClientAgent.sendMessage():\n"
					+ "Unable to create InternetAddress\n" + "From.Address="
					+ emailMsg._From.getAddress() + ", From.Name="
					+ emailMsg._From.getName() + ",\n" + "ReplyTo.Address="
					+ emailMsg._ReplyTo.getAddress() + ", ReplyTo.Name="
					+ emailMsg._ReplyTo.getName() + ",\n" + "CharSet="
					+ emailMsg._CharSet + ",\n" + "Exception info: "
					+ ex.toString();
			throw new EmailApplicationException(str);
		} catch (final Exception ex) {
			mLogger.severe(ex.getMessage());
			// sendAlert (EmailAlertCodes.EMAILEWAY_SEND_FAILED,
			// new String[] {configuration.getHostSend(),
			// Integer.toString(configuration.getPortSend())},
			// "Unable to send e-mail message due to general Exception. Please
			// see log file for details.",
			// NotificationEvent.SEVERITY_TYPE_CRITICAL);
			final String str = "EmailClientAgent.sendMessage():\n"
					+ "Unable to send message,\n" + "Exception info: "
					+ ex.toString();
			throw new EmailApplicationException(str);
		}

		retVal = true;

		return retVal;
	}

	/**
	 * Receive the next available message.
	 * 
	 * @param configuration
	 *            The EmailConfiguration instance containing connection specific
	 *            information.
	 * @param emailMsg
	 *            The EmailMessage to put the email message that gets received.
	 * 
	 * @return If there is/are more message(s) to be read then true will be
	 *         returned; otherwise, false will be returned.
	 * 
	 * @throws EmailApplicationException
	 *             upon error.
	 */
	public boolean receiveMessage(final EmailConfiguration configuration,
			final EmailMessage emailMsg) throws EmailApplicationException {

		if (configuration == null) {
			final String str = "EmailClientAgent.receiveMessage():\n"
					+ "Configuration for email is not provided; it is null";
			throw new EmailApplicationException(str);
		}

		if (emailMsg == null) {
			final String str = "EmailClientAgent.receiveMessage():\n"
					+ "EmailMessage is not provided; it is null";
			throw new EmailApplicationException(str);
		}

		final Session session = Session.getInstance(configuration.getProperties(),
				configuration.getAuthenticator());
		mLogger.fine("Got session instance.");

		if (mLogger.isLoggable(Level.FINE)) {
			session.setDebug(true);
		}

		Store store = null;
		try {
			// ... to read the first available message if any.
			store = session.getStore();

			if (null == store) {
				throw new MessagingException(
						"Unable to get message store for this session.");
			}
			mLogger.fine("getStore succeeded from session instance.");
			mLogger.fine("Connecting to store...");
			try {
				store.connect();
			} catch (final AuthenticationFailedException authEx) {
				mLogger.severe(authEx.getMessage());
				// sendAlert (EmailAlertCodes.EMAILEWAY_CONNECT_FAILED,
				// new String[] {configuration.getHostRecv(),
				// configuration.getProperties().getProperty("mail.store.protocol"),
				// Integer.toString(configuration.getPortRecv())},
				// "Failed to authenticate user " + configuration.getUserRecv()
				// + ".",
				// NotificationEvent.SEVERITY_TYPE_CRITICAL);
				throw authEx;
			} catch (final MessagingException msgEx) {
				mLogger.severe(msgEx.getMessage());
				// sendAlert (EmailAlertCodes.EMAILEWAY_CONNECT_FAILED,
				// new String[] {configuration.getHostRecv(),
				// configuration.getProperties().getProperty("mail.store.protocol"),
				// Integer.toString(configuration.getPortRecv())},
				// "Unable to connect due to MessagingException. Please see log
				// file for details.",
				// NotificationEvent.SEVERITY_TYPE_CRITICAL);
				throw msgEx;
			}

			final Folder folder = store.getFolder("INBOX");
			final String folderName = folder.getFullName();
			folder.open(Folder.READ_WRITE);
			final int msgCount = folder.getMessageCount();
			mLogger
					.fine("INBOX folder opened successfully for reading; MSG COUNT ["
							+ msgCount + "]");
			int i = 0;
			for (i = 1; i <= msgCount; i++) {
				// Get the next message
				final MimeMessage msg = (MimeMessage) folder.getMessage(i);

				mLogger.fine("Got one message (MimeMessage)");

				// Check if it is a ghost one ...
				if (msg.isExpunged()) {
					mLogger
							.fine("Message is expunged.. continue with next available message.");
					continue;
				}
				// It may be marked for deletion already ...
				if (msg.getFlags().contains(Flags.Flag.DELETED)) {
					mLogger
							.fine("Message is marked deleted.. continue with next available message.");
					continue;
				}
				// Process its content
				processInMessage(msg, emailMsg);
				// Mark it for deletion, so it can disappear after we close the
				// folder
				msg.setFlag(Flags.Flag.DELETED, true);
				mLogger
						.fine("Read Message successfully; message marked deleted.");
				break;
			}
			pop3HasMore = (msgCount - i) > 0;
			folder.close(true);
			// store.close(); // Move to finally to ensure it inbox will not be
			// locked in error condition
			mLogger.fine("INBOX folder closed and store closed.");

			mLogger.fine("receiveMessage returning " + pop3HasMore);
			return pop3HasMore;
		} catch (final MessagingException ex) {
			mLogger.severe(ex.getMessage());
			// sendAlert (EmailAlertCodes.EMAILEWAY_RECV_FAILED,
			// new String[] {configuration.getHostRecv(),
			// configuration.getProperties().getProperty("mail.store.protocol"),
			// Integer.toString(configuration.getPortRecv())},
			// "Error reading e-mail message due to MessagingException. Please
			// see log file for details.",
			// NotificationEvent.SEVERITY_TYPE_CRITICAL);
			final String str = "EmailClientAgent.receiveMessage():\n"
					+ "Unable to receive message,\n" + "Exception info: "
					+ ex.toString();
			throw new EmailApplicationException(str);
		} catch (final Exception ex) {
			mLogger.severe(ex.getMessage());
			// sendAlert (EmailAlertCodes.EMAILEWAY_RECV_FAILED,
			// new String[] {configuration.getHostRecv(),
			// configuration.getProperties().getProperty("mail.store.protocol"),
			// Integer.toString(configuration.getPortRecv())},
			// "Error reading e-mail message due to general Exception. Please
			// see log file for details.",
			// NotificationEvent.SEVERITY_TYPE_CRITICAL);
			final String str = "EmailClientAgent.receiveMessage():\n"
					+ "Unable to receive message,\n" + "Exception info: "
					+ ex.toString();
			throw new EmailApplicationException(str);
		} finally {
			if (store != null) {
				try {
					store.close();
				} catch (final MessagingException ex) {
					mLogger.severe(ex.getMessage());
					final String str = "EmailClientAgent.receiveMessage():\n"
							+ "Exception info: " + ex.toString();
					throw new EmailApplicationException(
							"Unable to close message store for this session: "
									+ str);
				}
			}
		}
	}

	/**
	 * Determines whether there are message(s) to be received.
	 * 
	 * @param configuration
	 *            The EmailConfiguration instance containing connection specific
	 *            information.
	 * 
	 * @return If there are messages to be read then true will be returned;
	 *         otherwise, false will be returned.
	 * 
	 * @throws EmailApplicationException
	 *             upon error.
	 */
	private boolean hasMessage(final EmailConfiguration configuration)
			throws EmailApplicationException {
		if (!pop3HasMore) {
			final Session session = Session.getInstance(
					configuration.getProperties(), configuration
							.getAuthenticator());
			mLogger.fine("Got session instance.");

			if (mLogger.isLoggable(Level.FINE)) {
				session.setDebug(true);
			}

			Store store = null;
			try {
				// ... to read the first available message if any.
				store = session.getStore();

				if (null == store) {
					throw new MessagingException(
							"Unable to get message store for this session.");
				}
				mLogger.fine("getStore succeeded from session instance.");
				mLogger.fine("Connecting to store...");
				try {
					store.connect();
				} catch (final AuthenticationFailedException authEx) {
					mLogger.severe(authEx.getMessage());
					// sendAlert (EmailAlertCodes.EMAILEWAY_CONNECT_FAILED,
					// new String[] {configuration.getHostRecv(),
					// configuration.getProperties().getProperty("mail.store.protocol"),
					// Integer.toString(configuration.getPortRecv())},
					// "Unable to authenticate user " +
					// configuration.getUserRecv() + ".",
					// NotificationEvent.SEVERITY_TYPE_CRITICAL);
					throw authEx;
				} catch (final MessagingException msgEx) {
					mLogger.severe(msgEx.getMessage());
					// sendAlert (EmailAlertCodes.EMAILEWAY_CONNECT_FAILED,
					// new String[] {configuration.getHostRecv(),
					// configuration.getProperties().getProperty("mail.store.protocol"),
					// Integer.toString(configuration.getPortRecv())},
					// "Unable to connect due to MessagingException. Please see
					// log file for details.",
					// NotificationEvent.SEVERITY_TYPE_CRITICAL);
					throw msgEx;
				}

				final Folder folder = store.getFolder("INBOX");
				final String folderName = folder.getFullName();
				folder.open(Folder.READ_WRITE);
				final int msgCount = folder.getMessageCount();
				mLogger
						.fine("INBOX folder opened successfully for reading; MSG COUNT ["
								+ msgCount + "]" + " in " + folderName);
				if (msgCount > 0) {
					pop3HasMore = true;
				}
				folder.close(true);
				// store.close(); // Move to finally to ensure it inbox will not
				// be locked in error condition
				mLogger.fine("INBOX folder closed and store closed.");
			} catch (final MessagingException ex) {
				mLogger.severe(ex.getMessage());
				// sendAlert (EmailAlertCodes.EMAILEWAY_CHECKEMAIL_FAILED,
				// new String[] {configuration.getHostRecv(),
				// configuration.getProperties().getProperty("mail.store.protocol"),
				// Integer.toString(configuration.getPortRecv())},
				// "Unable to check for e-mail messages due to
				// MessagingException. Please see log file for details.",
				// NotificationEvent.SEVERITY_TYPE_CRITICAL);
				final String str = "EmailClientAgent.hasMessages():\n"
						+ "Unable to check if there are messages,\n"
						+ "Exception info: " + ex.toString();
				throw new EmailApplicationException(str);
			} catch (final Exception ex) {
				mLogger.severe(ex.getMessage());
				// sendAlert(EmailAlertCodes.EMAILEWAY_CHECKEMAIL_FAILED,
				// new String[] {configuration.getHostRecv(),
				// configuration.getProperties().getProperty("mail.store.protocol"),
				// Integer.toString(configuration.getPortRecv())},
				// "Unable to check for e-mail messages due to general
				// Exception. Please see log file for details.",
				// NotificationEvent.SEVERITY_TYPE_CRITICAL);
				final String str = "EmailClientAgent.hasMessages():\n"
						+ "Unable to check if there are messages,\n"
						+ "Exception info: " + ex.toString();
				throw new EmailApplicationException(str);
			} finally {
				if (store != null) {
					try {
						store.close();
					} catch (final MessagingException ex) {
						mLogger.severe(ex.getMessage());
						final String str = "EmailClientAgent.hasMessages():\n"
								+ "Exception info: " + ex.toString();
						throw new EmailApplicationException(
								"Unable to close message store for this session: "
										+ str);
					}
				}
			}
		}

		mLogger.fine("hasMessage returning " + pop3HasMore);
		return pop3HasMore;
	}

	/*
	 * This method sends the failure notification back to the from address 
	 * where from the mail has originated.
	 */
	public void sendFailureNotification(final EmailConfiguration emailConfiguration,
			final EmailMessage emailMessage) {
		emailMessage.setSubject("Failed to send message to the recipients");
		// get the from address and clear the list
		// set the from address in the toaddress
		// send the failure notification.
		final EmailAddress[] fromAddress = {emailMessage.getFrom()};
		emailMessage.clearTo();
		emailMessage.clearBcc();
		emailMessage.clearCc();
		emailMessage.setTo(fromAddress);
		/*try{
			sendMessage(emailConfiguration,emailMessage);
		}catch(final EmailApplicationException eae){
			mLogger.log(Level.INFO, eae.getMessage());
		}*/
	}
}
