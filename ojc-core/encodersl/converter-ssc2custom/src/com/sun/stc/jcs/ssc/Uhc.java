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
 * @(#)Uhc.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcs.ssc;

import java.io.*;

import com.sun.stc.common.registry.RepositoryDirectories;
public class Uhc
{
    public static final char NoCode = 0xFFFD;
 public static char[] uhc2 ;
 static {
try 
 { 
  
RepositoryDirectories rpd = new RepositoryDirectories();
		  if (rpd.readRepositoryDirectories())
  {			  String path=rpd.getSharedExe();
			  path = path.replace('\\','/');
	uhc2= (char [])(new ObjectInputStream(new FileInputStream(path+"/../Server/registry/repository/default/i18tables/uhc.map"))).readObject();
	System.out.println("uhc"+uhc2[1]+uhc2[2]+uhc2[193]);
		  }
}
catch(Exception ex) 
 {  
	ex.printStackTrace(); 	
}
 } 

    // Positive maps 1-byte code, negative is index in
    // uhc2[] to map 2-byte UHC code to Unicode.
    public static final int[] uhc1 =
    {
	0x0000, 0x0001, 0x0002, 0x0003, 0x0004, 0x0005, 0x0006, 0x0007, 
	0x0008, 0x0009, 0x000A, 0x000B, 0x000C, 0x000D, 0x000E, 0x000F, 
	0x0010, 0x0011, 0x0012, 0x0013, 0x0014, 0x0015, 0x0016, 0x0017, 
	0x0018, 0x0019, 0x001A, 0x001B, 0x001C, 0x001D, 0x001E, 0x001F, 
	0x0020, 0x0021, 0x0022, 0x0023, 0x0024, 0x0025, 0x0026, 0x0027, 
	0x0028, 0x0029, 0x002A, 0x002B, 0x002C, 0x002D, 0x002E, 0x002F, 
	0x0030, 0x0031, 0x0032, 0x0033, 0x0034, 0x0035, 0x0036, 0x0037, 
	0x0038, 0x0039, 0x003A, 0x003B, 0x003C, 0x003D, 0x003E, 0x003F, 
	0x0040, 0x0041, 0x0042, 0x0043, 0x0044, 0x0045, 0x0046, 0x0047, 
	0x0048, 0x0049, 0x004A, 0x004B, 0x004C, 0x004D, 0x004E, 0x004F, 
	0x0050, 0x0051, 0x0052, 0x0053, 0x0054, 0x0055, 0x0056, 0x0057, 
	0x0058, 0x0059, 0x005A, 0x005B, 0x005C, 0x005D, 0x005E, 0x005F, 
	0x0060, 0x0061, 0x0062, 0x0063, 0x0064, 0x0065, 0x0066, 0x0067, 
	0x0068, 0x0069, 0x006A, 0x006B, 0x006C, 0x006D, 0x006E, 0x006F, 
	0x0070, 0x0071, 0x0072, 0x0073, 0x0074, 0x0075, 0x0076, 0x0077, 
	0x0078, 0x0079, 0x007A, 0x007B, 0x007C, 0x007D, 0x007E, 0x007F, 
	0x20AC, -1, -193, -385, -577, -769, -961, -1153, 
	-1345, -1537, -1729, -1921, -2113, -2305, -2497, -2689, 
	-2881, -3073, -3265, -3457, -3649, -3841, -4033, -4225, 
	-4417, -4609, -4801, -4993, -5185, -5377, -5569, -5761, 
	-5953, -6145, -6337, -6529, -6721, -6913, -7105, -7297, 
	-7489, -7681, -7873, -8065, -8257, -8449, -8641, -8833, 
	-9025, -9217, -9409, -9601, -9793, -9985, -10177, -10369, 
	-10561, -10753, -10945, -11137, -11329, -11521, -11713, -11905, 
	-12097, -12289, -12481, -12673, -12865, -13057, -13249, -13441, 
	-13633, -13825, -14017, -14209, -14401, -14593, -14785, -14977, 
	-15169, -15361, -15553, -15745, -15937, -16129, -16321, -16513, 
	-16705, -16897, -17089, -17281, -17473, -17665, -17857, -18049, 
	-18241, -18433, -18625, -18817, -19009, -19201, -19393, -19585, 
	-19777, -19969, -20161, -20353, -20545, -20737, -20929, -21121, 
	-21313, -21505, -21697, -21889, -22081, -22273, -22465, -22657, 
	-22849, -23041, -23233, -23425, -23617, -23809, -24001, NoCode
    };public static void main(String args[]){Uhc a= new Uhc();}
}
