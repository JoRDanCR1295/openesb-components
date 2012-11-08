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
 * $Id: StringContentVO.java,v 1.1.1.1 2007/04/09 17:49:27 mpreston Exp $
 */
package com.bostechcorp.cbesb.runtime.ccsl.errordb;

import javax.activation.DataHandler;

import com.bostechcorp.cbesb.runtime.ccsl.nmhandler.StringDataSource;

public class StringContentVO extends AbstractVO {
	private long exchangeId;
	private String type;
	private String name;
	private DataHandler value;

	public StringContentVO(long id, String type, String name, DataHandler value) {
		exchangeId = id;
		this.type = type;
		this.name = name;
		this.value = value;
	}
	
	public long getExchangeId() {
		return exchangeId;
	}
	
	public String getType() {
		return type;
	}
	
	public String getName() {
		return name;
	}
	
	public String getContent() {
		StringDataSource sds = (StringDataSource)value.getDataSource();
		return (sds != null) ? sds.getData() : /*null*/"";
	}
}
