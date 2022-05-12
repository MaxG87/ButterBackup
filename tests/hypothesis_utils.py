from hypothesis import strategies as st


@st.composite
def filenames(draw, min_size=1) -> str:
    alpha = "abcdefghijklmnopqrstuvwxyzäöu"
    num = "01234567890"
    special = "_-.,() "
    permitted_chars = f"{alpha}{alpha.upper()}{num}{special}"
    fname: str = draw(
        st.text(permitted_chars, min_size=min_size).filter(
            lambda fname: fname not in {".", ".."}
        )
    )
    return fname
