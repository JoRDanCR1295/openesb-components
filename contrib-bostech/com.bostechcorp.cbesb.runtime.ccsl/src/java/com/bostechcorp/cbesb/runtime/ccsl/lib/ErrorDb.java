/*
 * ChainBuilder ESB
 *          Visual Enterprise Integration
 * 
 * Copyright (C) 2006 Bostech Corporation
 * 
 * This program is free software; you can redistribute it and/or modify it 
 * under the terms of the GNU General Public License as published by the 
 * Free Software Foundation; either version 2 of the License, or (at your option) 
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, 
 * but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License 
 * for more details.
 * 
 * You should have received a copy of the GNU General Public License along with 
 * this program; if not, write to the Free Software Foundation, Inc., 
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 *
 * $Id: ErrorDb.java,v 1.1.1.1 2007/04/09 17:49:29 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.lib;

import java.sql.SQLException;
import java.util.Iterator;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.NormalizedMessage;

import org.apache.commons.logging.Log;

import com.bostechcorp.cbesb.runtime.ccsl.errordb.AttachmentVO;
import com.bostechcorp.cbesb.runtime.ccsl.errordb.ByteContentVO;
import com.bostechcorp.cbesb.runtime.ccsl.errordb.DaoConfig;
import com.bostechcorp.cbesb.runtime.ccsl.errordb.ErrorVO;
import com.bostechcorp.cbesb.runtime.ccsl.errordb.ExchangePropertyVO;
import com.bostechcorp.cbesb.runtime.ccsl.errordb.ExchangeVO;
import com.bostechcorp.cbesb.runtime.ccsl.errordb.MessagePropertyVO;
import com.bostechcorp.cbesb.runtime.ccsl.errordb.NormalizedMessageVO;
import com.bostechcorp.cbesb.runtime.ccsl.errordb.StringContentVO;
import com.bostechcorp.cbesb.runtime.ccsl.nmhandler.ByteArrayDataSource;
import com.bostechcorp.cbesb.runtime.ccsl.nmhandler.StringDataSource;
import com.ibatis.sqlmap.client.SqlMapClient;

public class ErrorDb {
	public static synchronized void write(Exception e, MessageExchange me, Log log) {
		long exchangeId = -1;
		log.info("writing message to error database");
		SqlMapClient sqlMap=null;
		try {
//System.out.println("getting sqlmapclient");
			DaoConfig daoConfig = new DaoConfig();
			sqlMap = daoConfig.getSqlMapInstance();
//System.out.println("starting transaction");
			sqlMap.startTransaction();
//System.out.println("OK");

			// write Exchange table
			ExchangeVO exchangeVO = new ExchangeVO(me);
//System.out.println("write exchange table");
			sqlMap.insert("insertExchange", exchangeVO);
			exchangeId = exchangeVO.getExchangeId();
			
			// write Error table
//System.out.println("write error table");
			ErrorVO errorVO = new ErrorVO(e, exchangeId);
			sqlMap.insert("insertError", errorVO);
			
			// write ExchangeProperty table
			for (Iterator i = me.getPropertyNames().iterator(); i.hasNext();) {
				String propertyName = (String)i.next();
//System.out.println("write property "+propertyName);
				Object propertyValue = me.getProperty(propertyName);
//System.out.println("value="+propertyValue);
				ExchangePropertyVO epvo = new ExchangePropertyVO(exchangeId, propertyName, propertyValue);
				sqlMap.insert("insertExchangeProperty", epvo);
//System.out.println("done");
			}
			
			// write NormalizedMessage table
			NormalizedMessage mess = null;
			NormalizedMessageVO messVO = null;
			final String[] messageTypes = {"in", "out", "fault"};
			for (int i=0; i < messageTypes.length; i++) {
//System.out.println("write "+messageTypes[i]+" message");
				mess = me.getMessage(messageTypes[i]);
				if (mess == null) continue;

				messVO = new NormalizedMessageVO(exchangeId, messageTypes[i], mess);
				sqlMap.insert("insertNormalizedMessage", messVO);				
				// write MessageProperty table
				for (Iterator it = mess.getPropertyNames().iterator(); it.hasNext();) {
					String propertyName = (String)it.next();
					Object propertyValue = mess.getProperty(propertyName);
					MessagePropertyVO mpvo = new MessagePropertyVO(exchangeId, messageTypes[i], propertyName, propertyValue);
					sqlMap.insert("insertMessageProperty", mpvo);				}
				
				// write Attachment table
				for (Iterator it = mess.getAttachmentNames().iterator(); it.hasNext();) {
					String attachmentName = (String)it.next();
					DataHandler dh = mess.getAttachment(attachmentName);
					DataSource ds = dh.getDataSource();
					String contentType = "unknown";
					if (ds instanceof StringDataSource) {
						contentType = "string";
					} else if (ds instanceof ByteArrayDataSource) {
						contentType = "byte";
					}

					AttachmentVO avo = new AttachmentVO(exchangeId, messageTypes[i], attachmentName, contentType);
					sqlMap.insert("insertAttachment", avo);
					
					if (ds instanceof StringDataSource) {
						// write StringContent table
						StringContentVO svo = new StringContentVO(exchangeId, messageTypes[i], attachmentName, dh);
						sqlMap.insert("insertStringContent", svo);
					} else if (ds instanceof ByteArrayDataSource) {
						// write ByteContent table
						ByteContentVO bvo = new ByteContentVO(exchangeId, messageTypes[i], attachmentName, dh);
						sqlMap.insert("insertByteContent", bvo);					}
				}
			}
			
//System.out.println("done writing error, committing\n\n\n");
			sqlMap.commitTransaction();
//System.out.println("OK\n\n\n");

		}
		catch (SQLException se) {
			log.error("\n\nSQLException writing error database "+se+"\n"+ExceptionUtil.stackTraceString(se));
		}
		finally {
System.out.println("end transaction sqlMap="+sqlMap);
			try {
				if (sqlMap != null) sqlMap.endTransaction();
			}
			catch (Exception e2) {
				log.error("error ending transaction: "+e2+"\n"+ExceptionUtil.stackTraceString(e2));
			}
System.out.println("OK");
		}
	}
}
