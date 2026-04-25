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


@st.composite
def valid_path_components(draw, min_size=1) -> str:
    name: str = draw(
        st.text(min_size=min_size).filter(
            lambda n: "/" not in n and "\x00" not in n and n not in {".", ".."}
        )
    )
    return name
