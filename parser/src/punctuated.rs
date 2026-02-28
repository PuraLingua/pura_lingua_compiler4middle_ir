use ast::punctuated::Punctuated;
use compiler_base::AnyResult;

use crate::cursor_mod::TokenCursor;

pub fn punctuated<'a, TValue, TSeparator, FValue, FSeparator>(
    cursor: &mut TokenCursor<'a>,
    err_when_nothing_matched: bool,
    f_value: FValue,
    f_separator: FSeparator,
) -> AnyResult<Punctuated<TValue, TSeparator>>
where
    FValue: Fn(&mut TokenCursor<'a>) -> AnyResult<TValue>,
    FSeparator: Fn(&mut TokenCursor<'a>) -> AnyResult<TSeparator>,
{
    let mut values = Vec::new();

    {
        let val = match f_value(cursor) {
            Ok(v) => v,
            Err(e) => {
                if err_when_nothing_matched {
                    return Err(e);
                } else {
                    return Ok(Punctuated::new(values, None));
                }
            }
        };
        match f_separator(cursor) {
            Ok(s) => {
                values.push((val, s));
            }
            Err(_) => return Ok(Punctuated::new(Vec::new(), Some(val))),
        }
    }

    while let Ok(v) = f_value(cursor) {
        match f_separator(cursor) {
            Ok(s) => {
                values.push((v, s));
            }
            Err(_) => {
                return Ok(Punctuated {
                    inner: values,
                    last: Some(v),
                });
            }
        }
    }

    Ok(Punctuated::new(values, None))
}
