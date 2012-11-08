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
 * @(#)DataConverter.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.bpel.xpath.functions;

import java.math.BigDecimal;
import java.math.BigInteger;

import javax.xml.namespace.QName;

import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

import com.sun.jbi.engine.bpel.core.bpel.dt.AnyURI;
import com.sun.jbi.engine.bpel.core.bpel.dt.Base64Binary;
import com.sun.jbi.engine.bpel.core.bpel.dt.DataTypes;
import com.sun.jbi.engine.bpel.core.bpel.dt.Date;
import com.sun.jbi.engine.bpel.core.bpel.dt.DateTime;
import com.sun.jbi.engine.bpel.core.bpel.dt.Duration;
import com.sun.jbi.engine.bpel.core.bpel.dt.GDay;
import com.sun.jbi.engine.bpel.core.bpel.dt.GMonth;
import com.sun.jbi.engine.bpel.core.bpel.dt.GMonthDay;
import com.sun.jbi.engine.bpel.core.bpel.dt.GYear;
import com.sun.jbi.engine.bpel.core.bpel.dt.GYearMonth;
import com.sun.jbi.engine.bpel.core.bpel.dt.HexBinary;
import com.sun.jbi.engine.bpel.core.bpel.dt.Notation;
import com.sun.jbi.engine.bpel.core.bpel.dt.Time;
import com.sun.jbi.engine.bpel.core.bpel.dt.impl.DataTypeFactoryImpl;


/**
 * Implementation of BPWSBytesToString
 */
class DataConverter {
    /** DOCUMENT ME! */
    public static String sep = System.getProperties().getProperty("line.separator");

    /** DOCUMENT ME! */
    public static final DataTypeFactoryImpl DTF = new DataTypeFactoryImpl();

    /**
     * DOCUMENT ME!
     *
     * @param src DOCUMENT ME!
     * @param iType DOCUMENT ME!
     * @param oType DOCUMENT ME!
     *
     * @return DOCUMENT ME!
     */
    public static Object convert(Object src, String iType, String oType) {
        // NOTE even though we get the iType we can not go by that
        // because src many not be the same instance as specified by the 
        // iType. For e.g. if the iType is duration, if src is a bean then 
        // it would be of type com.sun.jbi.engine.bpel.core.bpms.dt.Duration, else if it is 
        // DOM then src would be of instance Node/Element. That is why 
        // we need to use the instanceof operator
        if ((src != null) && (oType != null) && (iType != null) &&
                !iType.trim().equals("") && !oType.trim().equals("") &&
                !iType.trim().equalsIgnoreCase("anytype") &&
                !oType.trim().equalsIgnoreCase("anytype") &&
                !iType.trim().equalsIgnoreCase("anyuri") &&
                !oType.trim().equalsIgnoreCase("anyuri") &&
                !iType.trim().equalsIgnoreCase("notation") &&
                !oType.trim().equalsIgnoreCase("notation")) {
            Class fromType = DataTypes.getOutputTypeClass(iType);
            Class toType = DataTypes.getOutputTypeClass(oType);

            if ((fromType != null) && (toType != null)) {
                // Since all the boolean and toString convertions are same
                // we optimize it here, irrespective of the source type
                // Even though the individual converters handle these covertions
                // they never would be called as we short circuit the path here
                if ((toType == boolean.class) || (toType == Boolean.class) ||
                        (toType == String.class) || (src instanceof Duration) ||
                        (src instanceof Time) || (src instanceof GYear) ||
                        (src instanceof GDay) || (src instanceof GMonth) ||
                        (src instanceof AnyURI) || (src instanceof QName) ||
                        (src instanceof Notation)) {
                    String string = null;

                    if (src instanceof Node) {
                        string = getConcatTextValues((Element) src);
                    } else {
                        string = src.toString();
                    }

                    if (toType == String.class) {
                        return string;
                    }

                    if ((toType == boolean.class) || (toType == Boolean.class)) {
                        //See beyond's extension to xpath
                        if (string.equalsIgnoreCase("false")) {
                            return Boolean.valueOf(false);
                        }

                        // according to xpath a string with length >0 is always true
                        return  Boolean.valueOf(string.length() > 0);
                    }
                } else if (src instanceof Number) {
                    // Number (BigDecimal, BigInteger, Byte, Double, Float, Integer, Long, Short)
                    return convertFromNumber((Number) src, toType);
                } else if (src instanceof String) {
                    return convertFromString((String) src, fromType, toType);
                } else if (src instanceof Node) {
                    return convertFromString(getConcatTextValues((Element) src),
                        fromType, toType);
                } else if (src instanceof Boolean) {
                    return src.toString();
                } else if (src instanceof DateTime) {
                    return convertFromDateTime((DateTime) src, toType);
                } else if (src instanceof Date) {
                    return convertFromDate((Date) src, toType);
                } else if (src instanceof GYearMonth) {
                    return convertFromGYearMonth((GYearMonth) src, toType);
                } else if (src instanceof GMonthDay) {
                    return convertFromGMonthDay((GMonthDay) src, toType);
                } else if (src instanceof byte[]) {
                    return convertFromByteArray((byte[]) src, fromType, toType);
                }
            }
        }

        return src;
    }

    private static Object convertFromByteArray(byte[] src, Class fromType,
        Class toType) {
        if (src != null) {
            if (fromType == HexBinary.class) {
                if (toType == Base64Binary.class) {
                    return DTF.encodeToBase64Binary(src);
                }
            }

            /*
             * We do not support the byte[] source for Base64Binary to
                 if (fromType == Base64Binary.class) {
                 }
             */
        }

        return src;
    }

    private static Object convertFromGYearMonth(GYearMonth src, Class toType) {
        if (src != null) {
            if (toType == GMonth.class) {
                return DTF.newGMonthInstance(src.getMonth(), src.getTimeZone());
            }

            if (toType == GYear.class) {
                return DTF.newGYearInstance(src.getYear(), src.getTimeZone());
            }
        }

        return src;
    }

    private static Object convertFromDate(Date src, Class toType) {
        if (src != null) {
            if (toType == GMonth.class) {
                return DTF.newGMonthInstance(src.getMonth(), src.getTimeZone());
            }

            if (toType == GDay.class) {
                return DTF.newGDayInstance(src.getDay(), src.getTimeZone());
            }

            if (toType == GYear.class) {
                return DTF.newGYearInstance(src.getYear(), src.getTimeZone());
            }

            if (toType == GMonthDay.class) {
                return DTF.newGMonthDayInstance(src.getMonth(), src.getDay(),
                    src.getTimeZone());
            }

            if (toType == GYearMonth.class) {
                return DTF.newGYearMonthInstance(src.getYear(), src.getMonth(),
                    src.getTimeZone());
            }
        }

        return src;
    }

    private static Object convertFromGMonthDay(GMonthDay src, Class toType) {
        if (src != null) {
            if (toType == GMonth.class) {
                return DTF.newGMonthInstance(src.getMonth(), src.getTimeZone());
            }

            if (toType == GDay.class) {
                return DTF.newGDayInstance(src.getDay(), src.getTimeZone());
            }
        }

        return src;
    }

    private static Object convertFromDateTime(DateTime src, Class toType) {
        if (src != null) {
            if (toType == String.class) {
                return src.toString();
            }

            if ((toType == boolean.class) || (toType == Boolean.class)) {
                // according to xpath a string with length >0 is always true
                return Boolean.valueOf(src.toString().length() > 0);
            }

            if (toType == Time.class) {
                return DTF.newTimeInstance(src.getHours(), src.getMinutes(),
                    src.getSeconds(), src.getMilliseconds(), src.getTimeZone());
            }

            if (toType == Date.class) {
                return DTF.newDateInstance(src.getYear(), src.getMonth(),
                    src.getDay(), src.getTimeZone());
            }

            if (toType == GYearMonth.class) {
                return DTF.newGYearMonthInstance(src.getYear(), src.getMonth(),
                    src.getTimeZone());
            }

            if (toType == GYear.class) {
                return DTF.newGYearInstance(src.getYear(), src.getTimeZone());
            }

            if (toType == GMonthDay.class) {
                return DTF.newGMonthDayInstance(src.getMonth(), src.getDay(),
                    src.getTimeZone());
            }

            if (toType == GDay.class) {
                return DTF.newGDayInstance(src.getDay(), src.getTimeZone());
            }

            if (toType == GMonth.class) {
                return DTF.newGMonthInstance(src.getMonth(), src.getTimeZone());
            }
        }

        return src;
    }

    private static Object convertFromNumber(Number num, Class toType) {
        if (num != null) {
            if (toType == String.class) {
                return num.toString();
            }

            if ((toType == boolean.class) || (toType == Boolean.class)) {
                int i = num.intValue();

                if (i == 0) {
                    return Boolean.valueOf(false);
                } else if (i == 1) {
                    return Boolean.valueOf(true);
                }

                // according to xpath a string with length >0 is always true
                return Boolean.valueOf(num.toString().length() > 0);
            }

            if ((toType == char.class) || (toType == Character.class)) {
                return new Character(num.toString().charAt(0));
            }

            if ((toType == byte.class) || (toType == Byte.class)) {
                return new Byte(num.toString());
            }

            if ((toType == short.class) || (toType == Short.class)) {
                return new Short(num.shortValue());
            }

            if ((toType == int.class) || (toType == Integer.class)) {
                return new Integer(num.intValue());
            }

            if ((toType == long.class) || (toType == Long.class)) {
                return new Long(num.longValue());
            }

            if ((toType == float.class) || (toType == Float.class)) {
                return new Float(num.floatValue());
            }

            if ((toType == double.class) || (toType == Double.class)) {
                return new Double(num.doubleValue());
            }

            if (toType == BigDecimal.class) {
                return new BigDecimal(num.doubleValue());
            }

            if (toType == BigInteger.class) {
                return new BigInteger(num.toString());
            }
        }

        return num;
    }

    private static Number getNumberObject(String string, Class toType) {
        if ((toType == short.class) || (toType == Short.class)) {
            return new Short(string);
        }

        if ((toType == int.class) || (toType == Integer.class)) {
            return new Integer(string);
        }

        if ((toType == long.class) || (toType == Long.class)) {
            return new Long(string);
        }

        if ((toType == float.class) || (toType == Float.class)) {
            return new Float(string);
        }

        if ((toType == double.class) || (toType == Double.class)) {
            return new Double(string);
        }

        if (toType == BigDecimal.class) {
            return new BigDecimal(string);
        }

        if (toType == BigInteger.class) {
            return new BigInteger(string);
        }

        return null;
    }

    private static Object convertFromString(String string, Class fromType,
        Class toType) {
        if ((string == null) || (toType == String.class)) {
            return string;
        }

        if ((toType == boolean.class) || (toType == Boolean.class)) {
            if (string.equalsIgnoreCase("false")) {
                return Boolean.valueOf(false);
            }

            return Boolean.valueOf(string.length() > 0);
        }

        if ((toType == char.class) || (toType == Character.class)) {
            return new Character((string).charAt(0));
        }

        if ((toType == byte.class) || (toType == Byte.class)) {
            return new Byte(string);
        }

        if ((toType == short.class) || (toType == Short.class)) {
            if (fromType == String.class) {
                return new Short(string);
            }

            return convertFromNumber(getNumberObject(string, fromType), toType);
        }

        if ((toType == int.class) || (toType == Integer.class)) {
            if (fromType == String.class) {
                return new Integer(string);
            }

            return convertFromNumber(getNumberObject(string, fromType), toType);
        }

        if ((toType == long.class) || (toType == Long.class)) {
            if (fromType == String.class) {
                return new Long(string);
            }

            return convertFromNumber(getNumberObject(string, fromType), toType);
        }

        if ((toType == float.class) || (toType == Float.class)) {
            if (fromType == String.class) {
                return new Float(string);
            }

            return convertFromNumber(getNumberObject(string, fromType), toType);
        }

        if ((toType == double.class) || (toType == Double.class)) {
            if (fromType == String.class) {
                return new Double(string);
            }

            return convertFromNumber(getNumberObject(string, fromType), toType);
        }

        if (toType == BigDecimal.class) {
            if (fromType == String.class) {
                return new BigDecimal(string);
            }

            return convertFromNumber(getNumberObject(string, fromType), toType);
        }

        if (toType == BigInteger.class) {
            if (fromType == String.class) {
                return new BigInteger(string);
            }

            return convertFromNumber(getNumberObject(string, fromType), toType);
        }

        if (toType == Duration.class) {
            return DTF.parseDuration(string);
        }

        if (toType == DateTime.class) {
            if (fromType == String.class) {
                return DTF.parseDateTime(string);
            }
        }

        if (toType == Time.class) {
            if (fromType == String.class) {
                return DTF.parseTime(string);
            }

            if (fromType == DateTime.class) {
                return convertFromDateTime(DTF.parseDateTime(string), toType);
            }
        }

        if (toType == Duration.class) {
            if (fromType == String.class) {
                return DTF.parseDuration(string);
            }
        }

        if (toType == Date.class) {
            if (fromType == String.class) {
                return DTF.parseDate(string);
            }

            if (fromType == DateTime.class) {
                return convertFromDateTime(DTF.parseDateTime(string), toType);
            }
        }

        if (toType == GYearMonth.class) {
            if (fromType == String.class) {
                return DTF.parseGYearMonth(string);
            }

            if (fromType == DateTime.class) {
                return convertFromDateTime(DTF.parseDateTime(string), toType);
            }

            if (fromType == Date.class) {
                return convertFromDate(DTF.parseDate(string), toType);
            }
        }

        if (toType == GYear.class) {
            if (fromType == String.class) {
                return DTF.parseGYear(string);
            }

            if (fromType == DateTime.class) {
                return convertFromDateTime(DTF.parseDateTime(string), toType);
            }

            if (fromType == Date.class) {
                return convertFromDate(DTF.parseDate(string), toType);
            }

            if (fromType == GYearMonth.class) {
                return convertFromGYearMonth(DTF.parseGYearMonth(string), toType);
            }
        }

        if (toType == GMonthDay.class) {
            if (fromType == String.class) {
                return DTF.parseGMonthDay(string);
            }

            if (fromType == DateTime.class) {
                return convertFromDateTime(DTF.parseDateTime(string), toType);
            }

            if (fromType == Date.class) {
                return convertFromDate(DTF.parseDate(string), toType);
            }
        }

        if (toType == GDay.class) {
            if (fromType == String.class) {
                return DTF.parseGDay(string);
            }

            if (fromType == DateTime.class) {
                return convertFromDateTime(DTF.parseDateTime(string), toType);
            }

            if (fromType == Date.class) {
                return convertFromDate(DTF.parseDate(string), toType);
            }

            if (fromType == GMonthDay.class) {
                return convertFromGMonthDay(DTF.parseGMonthDay(string), toType);
            }
        }

        if (toType == GMonth.class) {
            if (fromType == String.class) {
                return DTF.parseGMonth(string);
            }

            if (fromType == DateTime.class) {
                return convertFromDateTime(DTF.parseDateTime(string), toType);
            }

            if (fromType == Date.class) {
                return convertFromDate(DTF.parseDate(string), toType);
            }

            if (fromType == GMonthDay.class) {
                return convertFromGMonthDay(DTF.parseGMonthDay(string), toType);
            }

            if (fromType == GYearMonth.class) {
                return convertFromGYearMonth(DTF.parseGYearMonth(string), toType);
            }
        }

        if (toType == Base64Binary.class) {
            return DTF.encodeToBase64Binary(string);
        }

        if (toType == HexBinary.class) {
            if (fromType == Base64Binary.class) {
                // decode the string and convert it to bytes
                return DTF.decodeFromBase64Binary(string);
            }

            // just convert the string to bytes
            return string.getBytes();
        }

        return string;
    }

    private static String getConcatTextValues(Element element) {
        NodeList nodeList = element.getChildNodes();

        String ret = null;

        int length = nodeList.getLength();

        for (int i = 0; i < length; i++) {
            Node node = nodeList.item(i);

            if (node instanceof Element) {
                // Find the inner text value by calling recursively
                String value = getConcatTextValues((Element) node);

                if (ret == null) {
                    ret = value;
                } else {
                    ret = ret + sep + value;
                }
            } else if (node instanceof Text) {
                int dl = ((Text) node).getLength();
                String value = null;

                if (dl > 0) {
                    value = ((Text) node).substringData(0, dl);

                    if ((value != null) && !value.trim().equals("") &&
                            !value.trim().equals("null")) {
                        if (ret == null) {
                            ret = value;
                        } else {
                            ret = (new StringBuffer(ret).append(sep).append(value)).toString();
                        }
                    }
                }

                return value;
            }
        }

        return ret;
    }
}
