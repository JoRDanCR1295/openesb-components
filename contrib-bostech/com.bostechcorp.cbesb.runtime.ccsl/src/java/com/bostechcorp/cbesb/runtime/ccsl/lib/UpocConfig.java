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
 * $Id: UpocConfig.java,v 1.1.1.1 2007/04/09 17:49:29 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.lib;

public class UpocConfig {
		private String type;
		private String className;
		private String method;
		private String rootDir;

		public void setType(String type) {
			// script type names are case sensitive and it appears to prefer lower.
			this.type = type.toLowerCase();
		}
		
		public void setClassName(String className) {
			this.className = className;
		}
		
		public void setMethod(String method) {
			this.method = method;
		}
		
		public String getType() {
			return type;
		}
		
		public String getClassName() {
			return className;
		}
		
		public String getMethod() {
			return method;
		}
		
		public void setRootDir(String dir) {
			rootDir = dir;
		}
		
		public String getRootDir() {
			return rootDir;
		}
}
