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
 * $Id: EndpointConfig.java,v 1.1.1.1 2007/04/09 17:49:29 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.lib;

import java.util.Hashtable;

public class EndpointConfig {
		private Hashtable<String, UpocConfig> upocs = new Hashtable<String, UpocConfig>();
		private boolean saveErrors;
		private boolean addRecord;
		private boolean stripRecord;
		private boolean sendMessage;
		private String suRoot;
		private boolean needToRunStart;
		private boolean needToRunStop;
		
		public void setSuRoot(String suRoot) {
			this.suRoot = suRoot;
		}
		
		public String getSuRoot() {
			return suRoot;
		}

		public void setSaveErrors(String saveErrors, boolean defaultVal) {
			if (saveErrors == null || saveErrors.length() == 0) 
				this.saveErrors = defaultVal;
			else
				this.saveErrors = Boolean.parseBoolean(saveErrors);
		}
		
		public void setAddRecord(String addRecord, boolean defaultVal) {
			if (addRecord == null || addRecord.length() == 0)
				this.addRecord = defaultVal;
			else
				this.addRecord = Boolean.parseBoolean(addRecord);
		}
		
		public void setStripRecord(String stripRecord, boolean defaultVal) {
			if (stripRecord == null || stripRecord.length() == 0)
				this.stripRecord = defaultVal;
			else
				this.stripRecord = Boolean.parseBoolean(stripRecord);
		}

		public void setSendMessage(String sendMessage, boolean defaultVal) {
			if (sendMessage == null || sendMessage.length() == 0) {
				this.sendMessage = defaultVal;
			} else {
				this.sendMessage = Boolean.parseBoolean(sendMessage);
			}
		}

		public boolean getSaveErrors() {
			return saveErrors;
		}
		
		public boolean getAddRecord() {
			return addRecord;
		}
		
		public boolean getStripRecord() {
			return stripRecord;
		}

		public boolean getSendMessage() {
			return sendMessage;
		}

		public void setNeedToRunStart(boolean val) {
			needToRunStart = val;
		}
		
		public boolean getNeedToRunStart() {
			return needToRunStart;
		}
		
		public void setNeedToRunStop(boolean val) {
			needToRunStop = val;
		}
		
		public boolean getNeedToRunStop() {
			return needToRunStop;
		}
		
		public void putUpoc(String key, UpocConfig upoc) {
			upocs.put(key, upoc);
		}
		
		public UpocConfig getUpoc(String key) {
			return (UpocConfig)upocs.get(key);
		}
}
