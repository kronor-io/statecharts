FROM postgres:18

RUN apt-get -qq update \
  && apt-get -qq --no-install-recommends install \
  sqitch \
  # needed by the semver extension
  make gcc postgresql-server-dev-18 \
  pgxnclient \
  curl ca-certificates \
  # needed to build rust extension
  libssl-dev pkg-config

RUN pgxn install semver

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

ENV PG_CONFIG=/usr/bin/pg_config

RUN cargo install cargo-pgrx --locked
RUN cargo pgrx init --pg18 /usr/bin/pg_config

COPY pg_statecharts /pg_statecharts
WORKDIR /pg_statecharts

RUN cargo pgrx install \
  --features pg18 \
  --no-default-features \
  --release \
  --pg-config /usr/bin/pg_config
